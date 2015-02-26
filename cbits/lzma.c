// #include <stdio.h>
#include <lzma.h>

void finalize_index(lzma_index *i)
{
	lzma_index_end(i, NULL);
}
