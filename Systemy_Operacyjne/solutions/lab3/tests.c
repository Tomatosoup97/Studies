#include <stdio.h>
#include <errno.h>
#include <limits.h>
#include "minunit.h"
#include "mem_mgmt.h"
#include "chunk.h"
#include "queue.h"

extern mem_ctl_t mem_ctl;

void test_setup(void) {
    // Do nothing
}

void test_teardown(void) {
    // Do nothing
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
    mu_check(calc_required_space(10) == PAGESIZE);
    mu_check(calc_required_space(PAGESIZE - sizeof(mem_chunk_t)) == PAGESIZE);
    mu_check(calc_required_space(PAGESIZE - sizeof(mem_chunk_t) + 1) == PAGESIZE * 2);
    mu_check(calc_required_space(PAGESIZE * 5) == PAGESIZE * 6);
}

MU_TEST(test_allocate_chunk) {
    LIST_INIT(&mem_ctl.ma_chunks); // reset list
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE * 2);
    uint64_t exp_chunk_size = PAGESIZE * 3 - sizeof(mem_chunk_t);
    uint64_t exp_fst_blk_size = exp_chunk_size - sizeof(mem_block_t);

    mu_check(chunk->size == exp_chunk_size);
    mu_check(chunk->ma_first->mb_size == exp_fst_blk_size);
    mu_check(LIST_FIRST(&chunk->ma_freeblks)->mb_size == chunk->ma_first->mb_size);
    mu_check(SND_FREE_BLK_IN_CHUNK(chunk) == NULL);
}

MU_TEST(test_find_chunk) {
    LIST_INIT(&mem_ctl.ma_chunks); // reset list
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
    LIST_INIT(&mem_ctl.ma_chunks); // reset list
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
    LIST_INIT(&mem_ctl.ma_chunks); // reset list

    allocate_chunk(PAGESIZE);
    mem_chunk_t *expected_chunk = allocate_chunk(PAGESIZE*3);
    allocate_chunk(256);

    mem_chunk_block_tuple_t *result = find_free_block_with_size(PAGESIZE*2);
    mu_check(result->chunk == expected_chunk);
    mu_check(result->block == expected_chunk->ma_first);
}

MU_TEST(test_find_free_block_with_size__return_null_when_no_block_found) {
    LIST_INIT(&mem_ctl.ma_chunks); // reset list

    allocate_chunk(PAGESIZE);
    allocate_chunk(PAGESIZE * 3 - sizeof(mem_chunk_t));
    allocate_chunk(256);

    mu_check(find_free_block_with_size(PAGESIZE * 3) == NULL);
}

MU_TEST(test_create_allocated_block) {
    mem_block_t *free_block = allocate_chunk(PAGESIZE - sizeof(mem_chunk_t))->ma_first;
    mem_block_t *alloc_block = create_allocated_block(free_block, 256);

    mu_check(alloc_block->mb_size == -256);
    mu_check(alloc_block->prev_block == free_block);
    mu_check((void*) alloc_block == \
             (void*) free_block + PAGESIZE - BOTH_METADATA_SIZE - 256);
}

MU_TEST(test_allocate_mem_in_block) {
    LIST_INIT(&mem_ctl.ma_chunks); // reset list
    size_t alloc_size = 256;
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE - sizeof(mem_chunk_t));
    mem_block_t *free_block = chunk->ma_first;
    allocate_mem_in_block(chunk, free_block, alloc_size);

    mu_check(free_block->prev_block == CANARY_ADDR);
    mu_check(FST_FREE_BLK_IN_CHUNK(chunk) == free_block);
    mu_check(SND_FREE_BLK_IN_CHUNK(chunk) == NULL);
    mu_check(free_block->mb_size == \
             PAGESIZE - BOTH_METADATA_SIZE - sizeof(mem_block_t) - alloc_size);
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
    MU_RUN_TEST(test_create_allocated_block);
    MU_RUN_TEST(test_allocate_mem_in_block);

}

int main() {
    MU_SUITE_CONFIGURE(&test_setup, &test_teardown);

    MU_RUN_SUITE(test_suite);
    MU_REPORT();
    return 0;
}
