#include <lzma.h>

void initialize_stream(lzma_stream *stream)
{
	lzma_stream tmp = LZMA_STREAM_INIT;
	*stream = tmp;
}
