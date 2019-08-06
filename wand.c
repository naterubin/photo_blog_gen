#include <stdio.h>
#include <stdlib.h>
#include <wand/MagickWand.h>
#include <libguile.h>

#define ThrowWandException(wand) \
  { \
    char \
      *description; \
    \
    ExceptionType \
      severity; \
    \
    description=MagickGetException(wand,&severity); \
    (void) fprintf(stderr,"%s %s %lu %s\n",GetMagickModule(),description); \
    description=(char *) MagickRelinquishMemory(description); \
    exit(-1); \
  }



void mw_resize(char* in, char* out, int columns, int rows) {
  MagickBooleanType status;
  MagickWand *magick_wand;

  MagickWandGenesis();
  magick_wand = NewMagickWand();
  status = MagickReadImage(magick_wand, in);
  if (status = MagickFalse)
    ThrowWandException(magick_wand);

  MagickResetIterator(magick_wand);
  while (MagickNextImage(magick_wand) != MagickFalse)
    MagickResizeImage(magick_wand, columns, rows, LanczosFilter, 1.0);

  status = MagickWriteImages(magick_wand, out, MagickTrue);
  if (status == MagickFalse)
    ThrowWandException(magick_wand);
  magick_wand = DestroyMagickWand(magick_wand);
  MagickWandTerminus();
}

SCM mw_getsize(SCM in) {
  MagickBooleanType status;
  MagickWand *magick_wand;
  size_t height, width;
  char* in_str = scm_to_locale_string(in);

  MagickWandGenesis();
  magick_wand = NewMagickWand();
  status = MagickReadImage(magick_wand, in_str);
  if (status = MagickFalse)
    ThrowWandException(magick_wand);

  MagickResetIterator(magick_wand);
  while (MagickNextImage(magick_wand) != MagickFalse) {
    height = MagickGetImageHeight(magick_wand);
    width = MagickGetImageWidth(magick_wand);
  }

  magick_wand = DestroyMagickWand(magick_wand);
  MagickWandTerminus();

  return scm_list_2(scm_from_int(width), scm_from_int(height));
}

SCM resize(SCM in, SCM out, SCM cols, SCM rows) {
  char* in_str = scm_to_locale_string(in);
  char* out_str = scm_to_locale_string(out);
  int cols_int = scm_to_int(cols);
  int rows_int = scm_to_int(rows);
  
  mw_resize(in_str, out_str, cols_int, rows_int);
  
  return SCM_BOOL_T;
}

void init_convert() {
  scm_c_define_gsubr("resize", 4, 0, 0, resize);
  scm_c_define_gsubr("img-size", 1, 0, 0, mw_getsize);
}
