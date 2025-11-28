/*
 * regcache.h - Unified register value cache
 */
#ifndef REGCACHE_H
#define REGCACHE_H

#include "cc2.h"

/* Invalidate individual register caches */
void cacheInvalA(void);
void cacheInvalHL(void);
void cacheInvalDE(void);
void cacheInvalBC(void);
void cacheInvalAll(void);

/* Set cache: register now holds expression e */
void cacheSetA(struct expr *e);
void cacheSetHL(struct expr *e);
void cacheSetDE(struct expr *e);
void cacheSetBC(struct expr *e);

/* Find which register holds expression e
 * Returns: 'A', 'H' (HL), 'D' (DE), 'B' (BC), or 0 if not cached
 */
int cacheFindByte(struct expr *e);
int cacheFindWord(struct expr *e);

/* Cache manipulation for stack operations */
void cacheSwapHLDE(void);
void cachePushHL(void);
void cachePopHL(void);

/* Debug access */
struct expr *cacheGetA(void);
struct expr *cacheGetHL(void);
struct expr *cacheGetDE(void);
struct expr *cacheGetBC(void);

#endif /* REGCACHE_H */
