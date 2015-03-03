#include <lzma.h>

void initialize_stream(lzma_stream *stream)
{
	lzma_stream tmp = LZMA_STREAM_INIT;
	*stream = tmp;
}

void finalize_index(lzma_index *index)
{
	lzma_index_end(index, NULL);
}
