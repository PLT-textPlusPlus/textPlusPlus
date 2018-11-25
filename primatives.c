#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <setjmp.h>
#include "hpdf.h"


HPDF_Doc pdf;
char fname[256];

HPDF_Font defaultFont;
HPDF_Page firstPage;
HPDF_Page currentPage;
int pageNumber;

HPDF_REAL pageHeight;
HPDF_REAL pageWidth;

HPDF_REAL currentX;
HPDF_REAL currentY;

HPDF_Font currentFont;
char *style;
HPDF_REAL currentSize;

jmp_buf env;
error_handler (HPDF_STATUS   error_no,
               HPDF_STATUS   detail_no,
               void         *user_data)
{
    printf ("ERROR: error_no=%04X, detail_no=%u\n", (HPDF_UINT)error_no,
                (HPDF_UINT)detail_no);
    longjmp(env, 1);
}



void startPDF(int arg1, char **arg2)
{   
    strcpy (fname, arg2[0]);
    strcat (fname, ".pdf");
    pdf = HPDF_New(error_handler, NULL); // creates pdf object
    defaultFont = HPDF_GetFont (pdf, "Helvetica", NULL); //loads Helvetica
    firstPage = HPDF_AddPage (pdf); //creates new page and adds it to end of document
    currentPage = firstPage;
    pageNumber = 1;
    pageHeight = HPDF_Page_GetHeight (firstPage); //gets page height
    pageWidth = HPDF_Page_GetWidth (firstPage); //gets page width
    HPDF_Page_SetFontAndSize(firstPage, defaulFont, NULL); //sets default font to page
    HPDF_Page_BeginText (firstPage); //begins text object and sets text position to (0, 0)
    currentX = 0;
    currentY = pageHeight;

}

void endPDF()
{
    HPDF_Page_EndText (currentPage); //ends a text object

    /* save the document to a file */
    HPDF_SaveToFile (pdf, fname);

    /* clean up */
    HPDF_Free (pdf);

}

void changeFontSize(char* font, int  size){

    HPDF_Page_SetFontAndSize (currentPage, font, size); 

}

void moveTo(int passedX, int passedY){

    HPDF_Page_MoveTextPos (currentPage, passedX, passedY);
}

void color(char* colorName){

    if( strcmp(colorName, "black") == 0 ){
        HPDF_Page_SetRGBFill (currentPage, 0.0, 0.0, 0.0);
    }
    if( strcmp(colorName, "red") == 0 ){
        HPDF_Page_SetRGBFill (currentPage, 1.0, 0.0, 0.0);
    }
    if( strcmp(colorName, "green") == 0 ){
        HPDF_Page_SetRGBFill (currentPage, 0.0, 1.0, 0.0);
    }
    if( strcmp(colorName, "blue") == 0 ){
        HPDF_Page_SetRGBFill (currentPage, 0.0, 0.0, 1.0);
    }

}

void addPage(){

    HPDF_Page newPage;
    pageNumber = pageNumber + 1;
    newPage = HPDF_AddPage (pdf);
    HPDF_Page_EndText (currentPage);
    currentPage = newPage;
    HPDF_Page_BeginText (currentPage);
    currentX = 0;
    currentY = pageHeight;

}

void drawLine(int beginX, float beginY, float offsetX, int offsetY){
    HPDF_Page_SetDash (currentPage, DASH_MODE3, 4, 0); 
    HPDF_Page_MoveTextPos (currentPage, beginX, beginY);
    HPDF_Page_LineTo (currentPage, beginX + offsetX, beginY - offsetY);
    HPDF_Page_Stroke (currentPage);
}

void write(char* text){
    HPDF_Page_TextOut (currentPage, currentX, currentY, text);
}


int main(int argc, char **argv)
{   char* fileName;

    if(argc != 2){
        fileName = "output";
    }
    else
    {
        fileName = argv[0];
    }

    startPDF(argc, &fileName);

    //ask where this should go

	endPDF();

    return 0;
}