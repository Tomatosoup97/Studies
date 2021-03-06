#include <sys/mman.h>
#include <stdio.h>
#include <errno.h>
#include <limits.h>
#include <assert.h>
#include <stdbool.h>
#include "minunit.h"
#include "mem_mgmt.h"
#include "chunk.h"
#include "queue.h"

extern mem_ctl_t mem_ctl;

void test_setup(void) {
    // Do nothing
}

void test_teardown(void) {
    /* Deallocate memory */
    mem_chunk_t *chunk;
    mem_block_t *block;
    mem_chunk_t *tmp_chunk;

    LIST_INIT(&mem_ctl.ma_chunks);

    FOR_EACH_CHUNK_SAFE(chunk, tmp_chunk) {
        // Make sure all canaries are valid
        block = chunk->ma_first;
        while (true) {
            CANARY_CHECK(block);
            if (IS_LAST_BLOCK(chunk, block)) break;
            block = GET_NEXT_BLOCK(chunk, block);
        }

        munmap(chunk, FULL_CHUNK_SIZE(chunk));
    }
}


/* TEST CHUNK OPERATIONS */

MU_TEST(test_align_size) {
    mu_check(ALIGN_SIZE(15, 16) == 16);
    mu_check(ALIGN_SIZE(32, 8) == 32);
    mu_check(ALIGN_SIZE(42, 8) == 48);
}

MU_TEST(test_calc_space_required) {
    size_t one_page_size = PAGESIZE - BOTH_METADATA_SIZE;
    mu_check(calc_required_space(10) == (size_t) PAGESIZE);
    mu_check(calc_required_space(one_page_size) == (size_t)PAGESIZE);
    mu_check(calc_required_space(one_page_size + 1) == (size_t) PAGESIZE * 2);
    mu_check(calc_required_space(PAGESIZE * 5) == (size_t) PAGESIZE * 6);
    mu_check(calc_required_space(PAGESIZE * 3 - BOTH_METADATA_SIZE) % PAGESIZE == 0);
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
    mu_check(FST_FREE_BLK_IN_CHUNK(chunk)->mb_size == chunk->ma_first->mb_size);
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

    mem_chunk_block_tuple_t result = find_free_block_with_size(PAGESIZE*2);

    mu_check(result.chunk == expected_chunk);
    mu_check(result.block == expected_chunk->ma_first);
}

MU_TEST(test_find_free_block_with_size__return_null_when_no_block_found) {
    allocate_chunk(PAGESIZE);
    allocate_chunk(PAGESIZE * 3 - BOTH_METADATA_SIZE);
    allocate_chunk(256);
    mem_chunk_block_tuple_t result = find_free_block_with_size(PAGESIZE * 3);

    mu_check(result.block == NULL);
    mu_check(result.chunk == NULL);
}

MU_TEST(test_create_allocated_block) {
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE - BOTH_METADATA_SIZE);
    mem_block_t *free_block = chunk->ma_first;
    mem_block_t *alloc_block = create_allocated_block(free_block, 256);

    mu_check(alloc_block->mb_size == -256);
    mu_check(alloc_block->prev_block == free_block);
    mu_check(alloc_block->magic_val == CANARY_ADDR);
    mu_check((void*) alloc_block == \
             (void*) free_block + PAGESIZE - BOTH_METADATA_SIZE - 256);
    mu_check(IS_LAST_BLOCK(chunk, alloc_block));
}

MU_TEST(test_allocate_mem_in_block) {
    size_t alloc_size = 256;
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE - BOTH_METADATA_SIZE);
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

    free_block(chunk, block_right->mb_data);
    mu_check(find_fst_prev_free_block(block_right) == block_left);

    mu_check(find_fst_prev_free_block(block_last) == block_right);

    // [ block_left                           | block_last ]
    free_block(chunk, block_mid->mb_data);

    mu_check(find_fst_prev_free_block(block_last) == block_left);
}

MU_TEST(test_left_coalesce_blocks) {
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*2 - BOTH_METADATA_SIZE);
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
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*5 - BOTH_METADATA_SIZE);
    mem_block_t *block_left = chunk->ma_first;
    mem_block_t *block_right = allocate_mem_in_block(chunk, block_left, size);
    mem_block_t *block_mid = allocate_mem_in_block(chunk, block_left, size);

    left_coalesce_blocks(chunk, block_left, block_mid);
    mu_check(block_right->prev_block == block_left);
}

MU_TEST(test_right_coalesce_blocks__one_free_block) {
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*2 - BOTH_METADATA_SIZE);
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

MU_TEST(test_shift_free_block_right) {
    // [(free) block_left | block_mid | (free) block_right ]
    size_t size = 256;
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*4 - sizeof(mem_chunk_t));
    mem_block_t *block_left = chunk->ma_first;
    mem_block_t *block_right = allocate_mem_in_block(chunk, block_left, size);
    mem_block_t *block_mid = allocate_mem_in_block(chunk, block_left, size);

    foo_free(block_right->mb_data);
    mu_check(LIST_NEXT(block_left, mb_node) == block_right);

    mem_block_t *shifted_block = shift_free_block_right(block_right, 16);

    mu_check(shifted_block->mb_size == (int64_t) (size - 16));
    mu_check(shifted_block->prev_block == block_mid);
    mu_check(LIST_NEXT(block_left, mb_node) == shifted_block);
}

MU_TEST(test_free_block) {
    // [ block_left | block_mid | block_right | block_last ]
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*5 - BOTH_METADATA_SIZE);
    mem_block_t *block_left = chunk->ma_first;
    mem_block_t *block_last = allocate_mem_in_block(chunk, block_left, 256);
    mem_block_t *block_right = allocate_mem_in_block(chunk, block_left, 256);
    mem_block_t *block_mid = allocate_mem_in_block(chunk, block_left, 256);
    int32_t initial_blk_size = PAGESIZE*5 - BOTH_METADATA_SIZE;
    int32_t alloc_bloc_size = FULL_BLOCK_SIZE(block_right);

    mu_check(block_left->mb_size == initial_blk_size - alloc_bloc_size*3);

    free_block(chunk, block_mid->mb_data);

    mu_check(FST_FREE_BLK_IN_CHUNK(chunk) == block_left);
    mu_check(SND_FREE_BLK_IN_CHUNK(chunk) == NULL);
    mu_check(block_left->mb_size == initial_blk_size - alloc_bloc_size*2);
    mu_check(!IS_BLOCK_FREE(block_right) && !IS_BLOCK_FREE(block_last));

    free_block(chunk, block_last->mb_data);

    mu_check(SND_FREE_BLK_IN_CHUNK(chunk) == block_last);
    mu_check(block_left->mb_size == initial_blk_size - alloc_bloc_size*2);

    free_block(chunk, block_right->mb_data);

    mu_check(FST_FREE_BLK_IN_CHUNK(chunk) == block_left);
    mu_check(SND_FREE_BLK_IN_CHUNK(chunk) == NULL);
    mu_check(IS_LAST_BLOCK(chunk, block_left));
    mu_check(block_left->mb_size == initial_blk_size);
}

/* TEST MEM_MGMT */

MU_TEST(test_posix_memalign_return_error_on_invalid_alignment) {
    void **ptr = NULL;
    mu_check(foo_posix_memalign(ptr, sizeof(void*)-1, 128) == EINVAL);
    mu_check(foo_posix_memalign(ptr, 200, 128) == EINVAL);
}

MU_TEST(test_is_power_of_two) {
    mu_check(IS_POWER_OF_TWO(64));
    mu_check(IS_POWER_OF_TWO(16));
    mu_check(!IS_POWER_OF_TWO(63));
    mu_check(!IS_POWER_OF_TWO(15));
}

MU_TEST(test_free) {
    // [block_left | block_mid | block_right ]
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*5 - sizeof(mem_chunk_t));
    mem_block_t *block_left = chunk->ma_first;
    mem_block_t *block_right = allocate_mem_in_block(chunk, block_left, 256);
    mem_block_t *block_mid = allocate_mem_in_block(chunk, block_left, 256);
    int32_t initial_block_left_size = block_left->mb_size;

    foo_free(block_right->mb_data);

    mu_check(SND_FREE_BLK_IN_CHUNK(chunk) == block_right);
    mu_check(initial_block_left_size == block_left->mb_size);
    mu_check(LIST_FIRST(&mem_ctl.ma_chunks) == chunk);

    foo_free(block_mid->mb_data);

    mu_check(LIST_FIRST(&mem_ctl.ma_chunks) == NULL);
}

MU_TEST(test_malloc) {
    int array_size = 5;
    int array[] = {1, 2, 3, 4, 5};
    int *rev_array = (int*) foo_malloc(array_size * sizeof(int));

    for (int i=array_size-1; i >= 0; i--)
        rev_array[i] = array[array_size - i - 1];

    mu_check(rev_array[array_size-1] == 1);
    mu_check(rev_array[0] == 5);
    mu_check(LIST_FIRST(&mem_ctl.ma_chunks) != NULL);

    foo_free(rev_array);

    mu_check(LIST_FIRST(&mem_ctl.ma_chunks) == NULL);
}

MU_TEST(test_malloc_big_space) {
    char *array1 = (char*) foo_malloc(4096 * 8);
    char *array2 = (char*) foo_malloc(4096*2);

    mu_check(LIST_FIRST(&mem_ctl.ma_chunks)->size == \
             (int64_t) (PAGESIZE * 3 - sizeof(mem_chunk_t)));

    mu_check(LIST_NEXT(LIST_FIRST(&mem_ctl.ma_chunks), ma_node)->size == \
             (int64_t) (PAGESIZE * 9 - sizeof(mem_chunk_t)));

    foo_free(array1);
    foo_free(array2);

    mu_check(LIST_FIRST(&mem_ctl.ma_chunks) == NULL);
}

MU_TEST(test_malloc__many_allocations) {
    const int count = 100;
    char *array[count];

    for (int i=0; i<count; i++)
        array[i] = foo_malloc(i * sizeof(char));

    for (int i=0; i<count; i++)
        foo_free(array[i]);

    mu_check(LIST_FIRST(&mem_ctl.ma_chunks) == NULL);
}

MU_TEST(test_calloc_fills_memory_with_zero) {
    int *array = (int*) foo_calloc(10, sizeof(int));

    for (int i=0; i<10; i++)
        mu_check(array[i] == 0);

    foo_free(array);
}

MU_TEST(test_calloc) {
    typedef struct point {
        int x;
        int y;
    } point_t;

    int32_t points_count = 4;
    point_t *points = foo_calloc(points_count, sizeof(point_t));
    int *results = (int*) foo_calloc(points_count, sizeof(int));
    int xs[4] = {2, 4, 8, 42};
    int ys[4] = {4, 1, 3, 1};

    for (int i=0; i < points_count; i++) {
        points->x = xs[i];
        points->y = ys[i];
        results[i] = points->x * points->y;
    }

    int expected_results[4] = {8, 4, 24, 42};
    for (int i=0; i < points_count; i++)
        mu_check(results[i] == expected_results[i]);

    mem_chunk_t *chunk = find_chunk(results);
    mem_block_t *results_block = find_block(results);
    mem_block_t *points_block = find_block(points);

    mu_check(chunk == find_chunk(points));
    mu_check(ABS(results_block->mb_size) == 64);
    mu_check(ABS(points_block->mb_size) == 64);
    mu_check(GET_NEXT_BLOCK(chunk, chunk->ma_first) == results_block);
    mu_check(GET_NEXT_BLOCK(chunk, results_block) == points_block);

    foo_free(results);

    mu_check(LIST_FIRST(&mem_ctl.ma_chunks) != NULL);

    foo_free(points);

    mu_check(LIST_FIRST(&mem_ctl.ma_chunks) == NULL);
}

MU_TEST(test_realloc__shrink_size) {
    void* current_mem = foo_malloc(1024);
    mem_block_t *prev_block = find_block(current_mem);

    size_t new_size = 512;
    void* realloc_mem = foo_realloc(current_mem, new_size);
    mem_block_t *realloc_block = find_block(realloc_mem);

    mu_check(realloc_block->magic_val == CANARY_ADDR);
    mu_check(prev_block == realloc_block);
    mu_check(realloc_block->mb_size == (-1) * (int64_t) new_size);
}

MU_TEST(test_realloc__alloc_in_new_place) {
    void* current_mem = foo_malloc(1024);
    mem_block_t *prev_block = find_block(current_mem);

    size_t new_size = PAGESIZE;
    void* realloc_mem = foo_realloc(current_mem, new_size);
    mem_chunk_t *realloc_chunk = find_chunk(realloc_mem);
    mem_block_t *realloc_block = find_block(realloc_mem);

    mu_check(prev_block != realloc_block);
    mu_check(realloc_block->mb_size == (-1) * PAGESIZE);

    mu_check(LIST_FIRST(&mem_ctl.ma_chunks) == realloc_chunk);
    mu_check(LIST_NEXT(LIST_FIRST(&mem_ctl.ma_chunks), ma_node) == NULL);
}

MU_TEST(test_realloc__take_mem_from_next_free_block) {
    size_t size = 256;
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE*4 - sizeof(mem_chunk_t));
    mem_block_t *block_left = chunk->ma_first;
    mem_block_t *block_right = allocate_mem_in_block(chunk, block_left, size);
    mem_block_t *block_mid = allocate_mem_in_block(chunk, block_left, size);

    foo_free(block_right->mb_data);

    void* realloc_ptr = foo_realloc(block_mid->mb_data, size + WORDSIZE);
    mem_block_t *shifted_block_right = LIST_NEXT(block_left, mb_node);

    mu_check(realloc_ptr == (void*) block_mid->mb_data);
    mu_check(block_mid->mb_size == (-1) * ((int64_t) (size + WORDSIZE)));
    mu_check(shifted_block_right->mb_size == (int64_t) (size - WORDSIZE));
    mu_check(shifted_block_right->prev_block == block_mid);
    mu_check(GET_NEXT_BLOCK(chunk, block_mid) == shifted_block_right);
}

/* CONFIG */

MU_TEST_SUITE(test_suite) {
    /* TEST CHUNK */
    MU_RUN_TEST(test_calc_space_required);
    MU_RUN_TEST(test_align_size);

    MU_RUN_TEST(test_allocate_chunk);
    MU_RUN_TEST(test_find_chunk);
    MU_RUN_TEST(test_free_chunk);

    /* TEST BLOCK */
    MU_RUN_TEST(test_create_allocated_block);
    MU_RUN_TEST(test_allocate_mem_in_block);
    MU_RUN_TEST(test_get_first_block);

    MU_RUN_TEST(test_find_free_block_with_size);
    MU_RUN_TEST(test_find_free_block_with_size__return_null_when_no_block_found);
    MU_RUN_TEST(test_find_fst_prev_free_block);
    MU_RUN_TEST(test_find_block);

    MU_RUN_TEST(test_left_coalesce_blocks);
    MU_RUN_TEST(test_left_coalesce_blocks__more_blocks);
    MU_RUN_TEST(test_right_coalesce_blocks__one_free_block);
    MU_RUN_TEST(test_right_coalesce_blocks);

    MU_RUN_TEST(test_shift_free_block_right);

    MU_RUN_TEST(test_free_block);

    /* TEST MEM_MGMT */
    MU_RUN_TEST(test_is_power_of_two);
    MU_RUN_TEST(test_posix_memalign_return_error_on_invalid_alignment);

    MU_RUN_TEST(test_malloc);
    MU_RUN_TEST(test_malloc_big_space);
    MU_RUN_TEST(test_malloc__many_allocations);

    MU_RUN_TEST(test_calloc);
    MU_RUN_TEST(test_calloc_fills_memory_with_zero);

    MU_RUN_TEST(test_free);

    MU_RUN_TEST(test_realloc__shrink_size);
    MU_RUN_TEST(test_realloc__alloc_in_new_place);
    MU_RUN_TEST(test_realloc__take_mem_from_next_free_block);
}

int main() {
    MU_SUITE_CONFIGURE(&test_setup, &test_teardown);

    MU_RUN_SUITE(test_suite);
    MU_REPORT();
    return 0;
}
