// #include <stdio.h>
#include <lzma.h>

void finalize_index(lzma_allocator *allocator, lzma_index *i)
{
	// puts("finalize_index");
	lzma_index_end(i, allocator);
}
