#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include <setjmp.h>
#include "hpdf.h"
#include "grid_sheet.h"

jmp_buf env;
error_handler (HPDF_STATUS   error_no,
               HPDF_STATUS   detail_no,
               void         *user_data)
{
    printf ("ERROR: error_no=%04X, detail_no=%u\n", (HPDF_UINT)error_no,
                (HPDF_UINT)detail_no);
    longjmp(env, 1);
}

void
show_description  (HPDF_Page          page,
                   HPDF_REAL          x,
                   HPDF_REAL          y,
                   const char   *text)
{
    float fsize = HPDF_Page_GetCurrentFontSize (page);
    HPDF_Font font = HPDF_Page_GetCurrentFont (page);
    HPDF_RGBColor c = HPDF_Page_GetRGBFill (page);

    HPDF_Page_BeginText (page);
    HPDF_Page_SetRGBFill (page, 0, 0, 0);
    HPDF_Page_SetTextRenderingMode (page, HPDF_FILL);
    HPDF_Page_SetFontAndSize (page, font, 10);
    HPDF_Page_TextOut (page, x, y - 12, text);
    HPDF_Page_EndText (page);

    HPDF_Page_SetFontAndSize (page, font, fsize);
    HPDF_Page_SetRGBFill (page, c.r, c.g, c.b);
}

int main(int argc, char **argv)
{
	HPDF_Doc pdf;
	HPDF_Font font;
	HPDF_Page page;
    float tw;

	const char *page_title = "Text Demo";

	const char* samp_text = "abcdefgABCDEFG123!#$&+-@?";

	pdf = HPDF_New (error_handler, NULL);
    if (!pdf) {
        printf ("error: cannot create PdfDoc object\n");
        return 1;
    }

    font = HPDF_GetFont (pdf, "Helvetica", NULL);

    page = HPDF_AddPage (pdf);

    HPDF_Page_SetFontAndSize (page, font, 24);
    tw = HPDF_Page_TextWidth (page, page_title);
    HPDF_Page_BeginText (page);
    HPDF_Page_TextOut (page, (HPDF_Page_GetWidth(page) - tw) / 2,
                HPDF_Page_GetHeight (page) - 50, page_title);
    HPDF_Page_EndText (page);

    /* save the document to a file */
    HPDF_SaveToFile (pdf, "text.pdf");

    /* clean up */
    HPDF_Free (pdf);

    return 0;
}