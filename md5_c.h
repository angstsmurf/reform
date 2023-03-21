#ifndef MD5_H
#define MD5_H

/*#include "HsFFI.h"*/
/*typedef HsWord32 uint32;*/

typedef unsigned int uint32;

struct MD5Context {
        uint32 buf[4];
        uint32 bits[2];
        unsigned char in[64];
};

void MD5Update(struct MD5Context *ctx, const unsigned char *buf, unsigned len);
void MD5UpdateRange(struct MD5Context *ctx, const unsigned char *buf, unsigned offset, unsigned len);
void MD5Final(unsigned char digest[16], struct MD5Context *ctx);
void MD5Transform(uint32 buf[4], uint32 in[16]);

#endif /* !MD5_H */
