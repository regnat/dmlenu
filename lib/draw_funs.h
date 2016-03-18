/* See LICENSE file for copyright and license details. */

#ifndef DRAW_FUNS_H
#define DRAW_FUNS_H

#define MAX(a,b)              ((a) > (b) ? (a) : (b))
#define MIN(a,b)              ((a) > (b) ? (b) : (a))
#define INTERSECT(x,y,w,h,r)  (MAX(0, MIN((x)+(w),(r).x_org+(r).width)  - MAX((x),(r).x_org)) \
                             * MAX(0, MIN((y)+(h),(r).y_org+(r).height) - MAX((y),(r).y_org)))
#define FG(dc, col)  ((col)[(dc)->invert ? ColBG : ColFG])
#define BG(dc, col)  ((col)[(dc)->invert ? ColFG : ColBG])
enum { ColBG, ColFG, ColBorder, ColLast };

typedef struct {
	int x, y, w, h;
	Bool invert;
	Display *dpy;
	GC gc;
	Pixmap canvas;
	struct {
		int ascent;
		int descent;
		int height;
		int width;
		XFontSet set;
		XFontStruct *xfont;
	} font;
} DC;  /* draw context */

void drawrect(DC *dc, int x, int y, unsigned int w, unsigned int h, Bool fill, unsigned long color);
size_t drawtext(DC *dc, const char *text, int, int,unsigned long col);
void freedc(DC *dc);
unsigned long getcolor(DC *dc, const char *colstr);
DC *initdc(void);
void initfont(DC *dc, const char *fontstr);
void mapdc(DC *dc, Window win, unsigned int w, unsigned int h);
void resizedc(DC *dc, unsigned int w, unsigned int h);
int textnw(DC *dc, const char *text, size_t len);
int textw(DC *dc, const char *text);

#endif
