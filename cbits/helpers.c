#include <lzma.h>

void lzma_init(lzma_stream *stream)
{
	lzma_stream tmp = LZMA_STREAM_INIT;
	*stream = tmp;
}
