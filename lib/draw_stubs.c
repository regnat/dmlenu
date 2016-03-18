#include <caml/callback.h>
#include <stdio.h>
#include <caml/mlvalues.h>
#include <caml/memory.h>
#include <caml/alloc.h>
#include <caml/custom.h>
#include <caml/fail.h>

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <unistd.h>
#ifdef XINERAMA
#include <X11/extensions/Xinerama.h>
#endif

#include "draw_funs.h"

static DC *dc = NULL;
static const char *font = NULL;
static Atom clip, utf8;
static Window win;
static XIC xic;
static int bh, mw, mh;
static int screen;
static int bottom = 0;
value 
caml_grabkeyboard(value unit) {
    CAMLparam1(unit);
    int i;
    /* try to grab keyboard, we may have to wait for another process to ungrab */
    for(i = 0; i < 1000; i++) {
        if(XGrabKeyboard(dc->dpy, DefaultRootWindow(dc->dpy), True,
                    GrabModeAsync, GrabModeAsync, CurrentTime) == GrabSuccess)
            CAMLreturn(Val_true);
        usleep(1000);
    }
    CAMLreturn(Val_false);
}

    value
caml_width(value unit)
{
    CAMLparam1(unit);
    CAMLreturn(Val_int(mw));
}

void
setup(int topbar, const char *bg, unsigned int lines) {
    int x = 0, y = 0; /* position of the window */
    if (!dc) {
        dc = initdc();
    }
    initfont(dc, font);
    screen = DefaultScreen(dc->dpy);
    Window root = RootWindow(dc->dpy, screen);
    XSetWindowAttributes swa;
    XIM xim;
#ifdef XINERAMA
    int n;
    XineramaScreenInfo *info;
#endif

    clip = XInternAtom(dc->dpy, "CLIPBOARD",   False);
    utf8 = XInternAtom(dc->dpy, "UTF8_STRING", False);

    /* calculate menu geometry */
    bh = dc->font.height + 2;
    lines = MAX(lines, 0);
    mh = (lines + 1) * bh;
#ifdef XINERAMA
    if((info = XineramaQueryScreens(dc->dpy, &n))) {
        int a, j, di, i = 0, area = 0;
        unsigned int du;
        Window w, pw, dw, *dws;
        XWindowAttributes wa;

        XGetInputFocus(dc->dpy, &w, &di);
        if(w != root && w != PointerRoot && w != None) {
            /* find top-level window containing current input focus */
            do {
                if(XQueryTree(dc->dpy, (pw = w), &dw, &w, &dws, &du) && dws)
                    XFree(dws);
            } while(w != root && w != pw);
            /* find xinerama screen with which the window intersects most */
            if(XGetWindowAttributes(dc->dpy, pw, &wa))
                for(j = 0; j < n; j++)
                    if((a = INTERSECT(wa.x, wa.y, wa.width, wa.height, info[j])) > area) {
                        area = a;
                        i = j;
                    }
        }
        /* no focused window is on screen, so use pointer location instead */
        if(!area && XQueryPointer(dc->dpy, root, &dw, &dw, &x, &y, &di, &di, &du))
            for(i = 0; i < n; i++)
                if(INTERSECT(x, y, 1, 1, info[i]))
                    break;

        x = info[i].x_org;
        y = info[i].y_org + (topbar ? 0 : info[i].height - mh);
        mw = info[i].width;
        XFree(info);
    }
    else
#endif
    {
        x = 0;
        y = topbar ? 0 : DisplayHeight(dc->dpy, screen) - mh;
        mw = DisplayWidth(dc->dpy, screen);
    }

    /* create menu window */
    swa.override_redirect = True;
    swa.background_pixel = getcolor(dc, bg);
    swa.event_mask = ExposureMask | KeyPressMask | VisibilityChangeMask;
    win = XCreateWindow(dc->dpy, root, x, y, mw, mh, 0,
            DefaultDepth(dc->dpy, screen), CopyFromParent,
            DefaultVisual(dc->dpy, screen),
            CWOverrideRedirect | CWBackPixel | CWEventMask, &swa);

    XResizeWindow(dc->dpy, win, mw, mh);
    /* open input methods */
    xim = XOpenIM(dc->dpy, NULL, NULL, NULL);
    xic = XCreateIC(xim, XNInputStyle, XIMPreeditNothing | XIMStatusNothing,
            XNClientWindow, win, XNFocusWindow, win, NULL);

    XMapRaised(dc->dpy, win);
    resizedc(dc, mw, mh);
    mapdc(dc, win, mw, mh);
}

    value 
caml_size (value string)
{
    CAMLparam1(string);
    CAMLreturn(Val_int (textw(dc, String_val(string))));
}
    value
caml_mapdc(value unit)
{
    CAMLparam1(unit);
    mapdc(dc, win, mw, mh);
    CAMLreturn(Val_unit);
}
    value
caml_clear(value bg)
{
    CAMLparam1(bg);
    drawrect(dc, 0, 0, mw, mh, True, getcolor(dc, String_val (bg)));
    CAMLreturn(Val_unit);
}

value
caml_clear_line(value cline, value bg)
{
    CAMLparam2(cline, bg);
    int line = Int_val(cline) ;
    if (bottom)
        /* magic formula */
        dc->y = mh - (bh - dc->font.ascent - 1 + line * bh);
    else
        dc->y = dc->font.ascent+1 + line * bh;
    drawrect(dc, 0, dc->y - dc->font.ascent - 1, mw, bh, True, getcolor(dc, String_val (bg)));
    CAMLreturn(Val_unit);
}

value
caml_resize(value line)
{
    CAMLparam1(line);
    mh = (Int_val(line) + 1) * bh;

    if(bottom) {
        XMoveWindow(dc->dpy, win, 0, DisplayHeight(dc->dpy, screen) - mh);
    }

    XResizeWindow(dc->dpy, win, mw, mh);
    resizedc(dc, mw, mh);
    CAMLreturn(Val_unit);
}

value
caml_drawtext(value string, value pos, value matches, value colors) {
    CAMLparam4(string, pos, matches, colors);
    size_t size = textw(dc, String_val (string));
    int x = Int_val(Field(pos, 0));
    if (bottom)
        /* magic formula */
        dc->y = mh - (bh - dc->font.ascent - 1 + Int_val(Field(pos, 1)) * bh);
    else
        dc->y = dc->font.ascent+1 + Int_val(Field(pos, 1)) * bh;
    drawrect(dc, x, dc->y-dc->font.ascent-1, size, bh, True, getcolor(dc, String_val(Field(colors, 2))));
    int start, stop;
    unsigned long fg;
    int xoff = x + dc->font.height/2;

    const char *str = String_val (string);
    value head;
    while (matches != Val_int(0)) {
        head = Field (matches, 0);
        matches = Field (matches, 1);
        dc->x = xoff;
        fg = getcolor(dc, String_val(Int_val(Field(head, 0)) == 0 ? Field(colors, 0) : Field(colors, 1)));
        start = Int_val(Field (head, 1));
        stop = Int_val(Field (head, 2));
        xoff += drawtext(dc, str, start, stop, fg);
    }
    CAMLreturn(Val_int(x + size));
}

value
caml_next_event(value unit) {
    CAMLparam1(unit);
    CAMLlocal1(mlev);
    XEvent ev;
    char buf[32];
    KeySym ksym = NoSymbol;
    Status status;
    size_t len;
    while(!XNextEvent(dc->dpy, &ev)) {
        if(XFilterEvent(&ev, win))
            continue;

        switch(ev.type) {
            case Expose:
                if(ev.xexpose.count == 0)
                    mapdc(dc, win, mw, mh);
                break;
            case KeyPress:
                len = XmbLookupString(xic, &ev.xkey, buf, sizeof buf, &ksym, &status);
                buf[len] = '\0';
                mlev = caml_alloc_tuple(2);
                Field(mlev, 0) = Val_int(ksym);
                Field(mlev, 1) = caml_copy_string (buf);
                CAMLreturn (mlev);
                break;
            case SelectionNotify:
                /* if(ev.xselection.property == utf8)
                        paste(); */
                break;
            case VisibilityNotify:
                if(ev.xvisibility.state != VisibilityUnobscured)
                    XRaiseWindow(dc->dpy, win);
                break;
        }
    }

    caml_failwith("unexpected return from XNextEvent") ;
}

value
caml_setup (value topbar, value bg, value lines)
{
    CAMLparam1(topbar);
    setup(Int_val(topbar), String_val (bg), Int_val (lines));
    bottom = !Int_val(topbar);
    CAMLreturn(Val_unit);
}

value
caml_xquit(value unit)
{
    CAMLparam1(unit);

    XUngrabKeyboard(dc->dpy, CurrentTime);
    XDestroyWindow(dc->dpy, win);
    XCloseDisplay(dc->dpy);
    CAMLreturn(Val_unit);
}
  
