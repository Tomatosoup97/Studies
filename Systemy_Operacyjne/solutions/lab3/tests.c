#include <stdio.h>
#include <errno.h>
#include <limits.h>
#include <assert.h>
#include "minunit.h"
#include "mem_mgmt.h"
#include "chunk.h"
#include "queue.h"

extern mem_ctl_t mem_ctl;

void test_setup(void) {
    LIST_INIT(&mem_ctl.ma_chunks); // reset list
}

void test_teardown(void) {
    // Do nothing
    // TODO: deallocate memory?
}

/* TEST MEM_MGMT */

MU_TEST(test_malloc_return_null_on_failure_and_set_proper_errno) {
    void *ptr = malloc(0xFFFFFFFFF);
    mu_check(ptr == NULL);
    mu_check(errno == ENOMEM);
}

MU_TEST(test_posix_memalign_return_error_on_invalid_alignment) {
    void **ptr = NULL;
    mu_check(posix_memalign(ptr, sizeof(void*)-1, 128) == EINVAL);
    mu_check(posix_memalign(ptr, 200, 128) == EINVAL);
}

MU_TEST(is_power_of_two) {
    mu_check(IS_POWER_OF_TWO(64));
    mu_check(IS_POWER_OF_TWO(16));
    mu_check(!IS_POWER_OF_TWO(63));
    mu_check(!IS_POWER_OF_TWO(15));
}

/* TEST CHUNK OPERATIONS */

MU_TEST(test_align_size) {
    mu_check(align_size(15, 16) == 16);
    mu_check(align_size(32, 8) == 32);
    mu_check(align_size(42, 8) == 48);
}

MU_TEST(test_calc_space_required) {
    size_t one_page_size = PAGESIZE - sizeof(mem_chunk_t);
    mu_check(calc_required_space(10) == (size_t) PAGESIZE);
    mu_check(calc_required_space(one_page_size) == (size_t)PAGESIZE);
    mu_check(calc_required_space(one_page_size + 1) == (size_t) PAGESIZE * 2);
    mu_check(calc_required_space(PAGESIZE * 5) == (size_t) PAGESIZE * 6);
}

MU_TEST(test_allocate_chunk) {
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE * 2);
    int32_t exp_chunk_size = PAGESIZE * 3 - sizeof(mem_chunk_t);
    int32_t exp_fst_blk_size = exp_chunk_size - sizeof(mem_block_t);

    mu_check(BLOCK_END_ADDR(chunk->ma_first) == CHUNK_END_ADDR(chunk));
    mu_check(chunk->size == exp_chunk_size);
    mu_check(chunk->ma_first->mb_size == exp_fst_blk_size);
    mu_check(chunk->ma_first->magic_val == CANARY_ADDR);
    mu_check(chunk->ma_first->prev_block == NULL);
    mu_check(LIST_FIRST(&chunk->ma_freeblks)->mb_size == chunk->ma_first->mb_size);
    mu_check(SND_FREE_BLK_IN_CHUNK(chunk) == NULL);
}

MU_TEST(test_find_chunk) {
    allocate_chunk(2048);
    mem_chunk_t *chunk1 = allocate_chunk(PAGESIZE - sizeof(mem_chunk_t));
    mem_chunk_t *chunk2 = allocate_chunk(PAGESIZE * 2);
    allocate_chunk(512);

    mu_check(find_chunk((void*) chunk1 + BOTH_METADATA_SIZE + 16) == chunk1);
    mu_check(find_chunk((void*) chunk1 + PAGESIZE - 1) == chunk1);
    mu_check(find_chunk((void*) chunk2 + 1024) == chunk2);
    mu_check(find_chunk((void*) 0xFFFFFFFF) == NULL);
}

MU_TEST(test_free_chunk) {
    // Create [chunk3] -> [chunk2] -> [chunk1]
    mem_chunk_t *chunk1 = allocate_chunk(64);
    mem_chunk_t *chunk2 = allocate_chunk(PAGESIZE);
    mem_chunk_t *chunk3 = allocate_chunk(PAGESIZE * 2);

    free_chunk(chunk2);
    mem_chunk_t *fst_chunk = LIST_FIRST(&mem_ctl.ma_chunks);
    // Assert [chunk3] -> [chunk1]
    mu_check(
            fst_chunk == chunk3 &&
            LIST_NEXT(fst_chunk, ma_node) == chunk1 &&
            LIST_NEXT(LIST_NEXT(fst_chunk, ma_node), ma_node) == NULL
    );

    free_chunk(chunk1);
    fst_chunk = LIST_FIRST(&mem_ctl.ma_chunks);
    // Assert [chunk3]
    mu_check( fst_chunk == chunk3 && LIST_NEXT(fst_chunk, ma_node) == NULL );
}

/* TEST BLOCK OPERATIONS */

MU_TEST(test_find_free_block_with_size) {
    allocate_chunk(PAGESIZE);
    mem_chunk_t *expected_chunk = allocate_chunk(PAGESIZE*3);
    allocate_chunk(256);

    mem_chunk_block_tuple_t *result = find_free_block_with_size(PAGESIZE*2);

    mu_check(result->chunk == expected_chunk);
    mu_check(result->block == expected_chunk->ma_first);
}

MU_TEST(test_find_free_block_with_size__return_null_when_no_block_found) {
    allocate_chunk(PAGESIZE);
    allocate_chunk(PAGESIZE * 3 - sizeof(mem_chunk_t));
    allocate_chunk(256);

    mu_check(find_free_block_with_size(PAGESIZE * 3) == NULL);
}

MU_TEST(test_create_allocated_block) {
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE - sizeof(mem_chunk_t));
    mem_block_t *free_block = chunk->ma_first;
    mem_block_t *alloc_block = create_allocated_block(free_block, 256);

    mu_check(alloc_block->mb_size == -256);
    mu_check(alloc_block->prev_block == free_block);
    mu_check(alloc_block->magic_val == CANARY_ADDR);
    mu_check((void*) alloc_block == \
             (void*) free_block + PAGESIZE - BOTH_METADATA_SIZE - 256);
}

MU_TEST(test_allocate_mem_in_block) {
    size_t alloc_size = 256;
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE - sizeof(mem_chunk_t));
    mem_block_t *free_block = chunk->ma_first;
    mem_block_t *alloc_block = allocate_mem_in_block(chunk, free_block, alloc_size);

    void *end_addr = (void*) (BLOCK_END_ADDR(free_block) + (FULL_BLOCK_SIZE(alloc_block)));

    int32_t exp_free_blk_size = PAGESIZE - BOTH_METADATA_SIZE \
                              - sizeof(mem_block_t) - alloc_size;

    mu_check(IS_LAST_BLOCK(chunk, alloc_block));
    mu_check(!IS_LAST_BLOCK(chunk, free_block));
    mu_check(GET_NEXT_BLOCK(chunk, free_block) == alloc_block);
    mu_check((void*) (free_block + FULL_BLOCK_SIZE(free_block)));
    mu_check(end_addr == CHUNK_END_ADDR(chunk));
    mu_check((void*) alloc_block == (void*) BLOCK_END_ADDR(chunk->ma_first));
    mu_check(FST_FREE_BLK_IN_CHUNK(chunk) == free_block);
    mu_check(SND_FREE_BLK_IN_CHUNK(chunk) == NULL);
    mu_check(free_block->prev_block == NULL);
    mu_check(free_block->mb_size == exp_free_blk_size);

    mem_block_t *alloc_block2 = allocate_mem_in_block(chunk, free_block, alloc_size);

    mu_check(alloc_block2->prev_block == free_block);
    mu_check(alloc_block->prev_block == alloc_block2);
}

MU_TEST(test_find_block) {
    allocate_chunk(2048);
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE - sizeof(mem_chunk_t));
    mem_chunk_t *chunk2 = allocate_chunk(PAGESIZE * 2);
    allocate_chunk(512);

    mem_block_t *alloc_block = allocate_mem_in_block(chunk2, chunk2->ma_first, 256);

    mu_check(find_block((void*) chunk->ma_first->mb_data) == chunk->ma_first);
    mu_check(find_block((void*) (alloc_block->mb_data)) == GET_NEXT_BLOCK(chunk2, chunk2->ma_first));
}

MU_TEST(test_get_first_block) {
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*5 - sizeof(mem_chunk_t));
    mem_block_t *free_block = chunk->ma_first;
    mem_block_t *alloc_block1 = allocate_mem_in_block(chunk, free_block, 256);
    mem_block_t *alloc_block2 = allocate_mem_in_block(chunk, free_block, 256);
    mem_block_t *alloc_block3 = allocate_mem_in_block(chunk, free_block, 256);

    mu_check(get_first_block(alloc_block1) == free_block);
    mu_check(get_first_block(alloc_block2) == free_block);
    mu_check(get_first_block(alloc_block3) == free_block);
}

MU_TEST(test_find_fst_prev_free_block) {
    // [ block_left | block_mid | block_right | block_last ]
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*5 - sizeof(mem_chunk_t));
    mem_block_t *block_left = chunk->ma_first;
    mem_block_t *block_last = allocate_mem_in_block(chunk, block_left, 256);
    mem_block_t *block_right = allocate_mem_in_block(chunk, block_left, 256);
    mem_block_t *block_mid = allocate_mem_in_block(chunk, block_left, 256);

    mu_check(find_fst_prev_free_block(block_right) == block_left);
    mu_check(find_fst_prev_free_block(block_mid) == block_left);

    free_block(block_right->mb_data);
    mu_check(find_fst_prev_free_block(block_right) == block_left);

    mu_check(find_fst_prev_free_block(block_last) == block_right);

//     [ block_left                           | block_last ]
    free_block(block_mid->mb_data);

    mu_check(find_fst_prev_free_block(block_last) == block_left);
}

MU_TEST(test_left_coalesce_blocks) {
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*2 - sizeof(mem_chunk_t));
    mem_block_t *free_block = chunk->ma_first;
    mem_block_t *alloc_block = allocate_mem_in_block(chunk, free_block, 256);

    left_coalesce_blocks(chunk, free_block, alloc_block);
    mu_check(FST_FREE_BLK_IN_CHUNK(chunk)->mb_size == \
             (int32_t) (PAGESIZE*2 - BOTH_METADATA_SIZE));
    mu_check(IS_LAST_BLOCK(chunk, free_block));
}

MU_TEST(test_left_coalesce_blocks__more_blocks) {
    // [ block_left | block_mid | block_right ]
    size_t size = 256;
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*5 - sizeof(mem_chunk_t));
    mem_block_t *block_left = chunk->ma_first;
    mem_block_t *block_right = allocate_mem_in_block(chunk, block_left, size);
    mem_block_t *block_mid = allocate_mem_in_block(chunk, block_left, size);

    left_coalesce_blocks(chunk, block_left, block_mid);
    mu_check(block_right->prev_block == block_left);
}

MU_TEST(test_right_coalesce_blocks__one_free_block) {
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*2 - sizeof(mem_chunk_t));
    mem_block_t *left_block = chunk->ma_first;
    mem_block_t *right_block = allocate_mem_in_block(chunk, left_block, 256);
    right_block->mb_size = ABS(right_block->mb_size);
    left_block->mb_size *= (-1);
    LIST_REMOVE(left_block, mb_node);
    LIST_INSERT_HEAD(&chunk->ma_freeblks, right_block, mb_node);

    right_coalesce_blocks(left_block, right_block);

    mu_check(FST_FREE_BLK_IN_CHUNK(chunk)->mb_size == \
             (int32_t) (PAGESIZE*2 - BOTH_METADATA_SIZE));
    mu_check(IS_LAST_BLOCK(chunk, right_block));
}

MU_TEST(test_right_coalesce_blocks) {
    // [ block_left | block_mid | block_right ]
    size_t size = 256;
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*5 - sizeof(mem_chunk_t));
    mem_block_t *block_left = chunk->ma_first;
    mem_block_t *block_right = allocate_mem_in_block(chunk, block_left, size);
    mem_block_t *block_mid = allocate_mem_in_block(chunk, block_left, size);

    LIST_INSERT_AFTER(block_left, block_right, mb_node);
    block_right->mb_size *= (-1);

    right_coalesce_blocks(block_mid, block_right);

    mu_check(SND_FREE_BLK_IN_CHUNK(chunk) == block_mid);
    mu_check(LIST_NEXT(block_mid, mb_node) == NULL);
    mu_check(IS_LAST_BLOCK(chunk, block_mid));
    mu_check(block_mid->mb_size == (int32_t) (size + FULL_BLOCK_SIZE(block_right)));
}

MU_TEST(test_free_block) {
    // [ block_left | block_mid | block_right | block_last ]
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*5 - sizeof(mem_chunk_t));
    mem_block_t *block_left = chunk->ma_first;
    mem_block_t *block_last = allocate_mem_in_block(chunk, block_left, 256);
    mem_block_t *block_right = allocate_mem_in_block(chunk, block_left, 256);
    mem_block_t *block_mid = allocate_mem_in_block(chunk, block_left, 256);
    int32_t initial_blk_size = PAGESIZE*5 - BOTH_METADATA_SIZE;
    int32_t alloc_bloc_size = FULL_BLOCK_SIZE(block_right);

    mu_check(block_left->mb_size == initial_blk_size - alloc_bloc_size*3);

    free_block(block_mid->mb_data);

    mu_check(FST_FREE_BLK_IN_CHUNK(chunk) == block_left);
    mu_check(SND_FREE_BLK_IN_CHUNK(chunk) == NULL);
    mu_check(block_left->mb_size == initial_blk_size - alloc_bloc_size*2);
    mu_check(!IS_BLOCK_FREE(block_right) && !IS_BLOCK_FREE(block_last));

    free_block(block_last->mb_data);

    mu_check(SND_FREE_BLK_IN_CHUNK(chunk) == block_last);
    mu_check(block_left->mb_size == initial_blk_size - alloc_bloc_size*2);

    free_block(block_right->mb_data);

    mu_check(FST_FREE_BLK_IN_CHUNK(chunk) == block_left);
    mu_check(SND_FREE_BLK_IN_CHUNK(chunk) == NULL);
    mu_check(IS_LAST_BLOCK(chunk, block_left));
    mu_check(block_left->mb_size == initial_blk_size);
}

/* CONFIG */

MU_TEST_SUITE(test_suite) {
    /* TEST MEM_MGMT */
    MU_RUN_TEST(test_malloc_return_null_on_failure_and_set_proper_errno);
    MU_RUN_TEST(test_posix_memalign_return_error_on_invalid_alignment);
    MU_RUN_TEST(is_power_of_two);

    /* TEST CHUNK */
    MU_RUN_TEST(test_calc_space_required);
    MU_RUN_TEST(test_allocate_chunk);
    MU_RUN_TEST(test_align_size);
    MU_RUN_TEST(test_find_chunk);
    MU_RUN_TEST(test_free_chunk);

    /* TEST BLOCK */
    MU_RUN_TEST(test_find_free_block_with_size);
    MU_RUN_TEST(test_find_free_block_with_size__return_null_when_no_block_found);
    MU_RUN_TEST(test_find_fst_prev_free_block);
    MU_RUN_TEST(test_find_block);
    MU_RUN_TEST(test_create_allocated_block);
    MU_RUN_TEST(test_allocate_mem_in_block);
    MU_RUN_TEST(test_left_coalesce_blocks);
    MU_RUN_TEST(test_left_coalesce_blocks__more_blocks);
    MU_RUN_TEST(test_right_coalesce_blocks__one_free_block);
    MU_RUN_TEST(test_right_coalesce_blocks);
    MU_RUN_TEST(test_get_first_block);
    MU_RUN_TEST(test_free_block);
}

int main() {
    MU_SUITE_CONFIGURE(&test_setup, &test_teardown);

    MU_RUN_SUITE(test_suite);
    MU_REPORT();
    return 0;
}
