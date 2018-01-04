#include <stdio.h>
#include <errno.h>
#include <limits.h>
#include "minunit.h"
#include "mem_mgmt.h"
#include "chunk.h"

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

/* TEST CHUNK */

MU_TEST(test_calc_space_required) {
    mu_check(calc_required_space(10) == PAGESIZE);
    mu_check(calc_required_space(PAGESIZE - sizeof(mem_chunk_t)) == PAGESIZE);
    mu_check(calc_required_space(PAGESIZE - sizeof(mem_chunk_t) + 1) == PAGESIZE * 2);
    mu_check(calc_required_space(PAGESIZE * 5) == PAGESIZE * 6);
}

MU_TEST(test_allocate_chunk) {
    mem_chunk_t *chunk = allocate_chunk(PAGESIZE * 2);
    uint64_t exp_chunk_size = PAGESIZE * 3 - sizeof(mem_chunk_t);

    mu_check(chunk->size == exp_chunk_size);
    // assert first block has expected size
    mu_check(chunk->ma_first->mb_size == exp_chunk_size - sizeof(mem_block_t));
    // assert list and ma_first are the same
    mu_check(LIST_FIRST(&chunk->ma_freeblks)->mb_size == chunk->ma_first->mb_size);
    // assert list has one element
    mu_check( LIST_NEXT(LIST_FIRST(&chunk->ma_freeblks), mb_node) == NULL );
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
}

int main() {
    MU_SUITE_CONFIGURE(&test_setup, &test_teardown);

    MU_RUN_SUITE(test_suite);
    MU_REPORT();
    mdump();
    return 0;
}
