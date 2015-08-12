#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <ctype.h>
#include <getopt.h>

#include "wand/magick_wand.h"

void
wordWrap(const char* text, int columns, char *buf)
{
  int length;
  int characters_per_line;
  int i = 0;
  int j = 0;
  int curr = 0;

  length = strlen(text);
  characters_per_line = columns + 4;
  while (i < length) {
    if ((curr < characters_per_line) ||
	(curr >= characters_per_line && !isspace(text[i]))) {
      buf[j] = text[i];
      curr++;
      j++;
    } else {
      buf[j] = text[i];
      buf[j+1] = '\n';
      curr = 0;
      j = j + 2;
    }
    i++;
  }
}

double
scaleText(const char* text, char* buf)
{
  int length = strlen(text);
  if(length < 10) {
    strcpy(buf, text);
    return 1.0;
  } else if(length < 24) {
    wordWrap(text, 10, buf);
    return 0.70;
  } else if(length < 48) {
    wordWrap(text, 15, buf);
    return 0.5;
  } else {
    wordWrap(text, 20, buf);
    return 0.40;
  }
}

void
generate(char *source_image_path, char *sink_image_path, char *meme_text)
{

  MagickWand *wand = NULL;
  DrawingWand *drawing_wand = NULL;
  PixelWand *pixel_wand = NULL;

  MagickWandGenesis();

  wand = NewMagickWand();
  drawing_wand = NewDrawingWand();
  pixel_wand = NewPixelWand();

  // read base image
  MagickReadImage(wand, source_image_path);

  ssize_t width;
  ssize_t height;
  ssize_t pointsize;
  ssize_t stroke_width;
  double scale;
  char formatted_text[100] = { '\0' };

  // Scale text
  width = MagickGetImageWidth(wand);
  height = MagickGetImageHeight(wand);
  scale = scaleText(meme_text, formatted_text);
  pointsize = width / 5.0;
  stroke_width = pointsize / 30.0;

  // Draw base text
  PixelSetColor(pixel_wand, "white");
  DrawSetFillColor(drawing_wand, pixel_wand);
  DrawSetFont(drawing_wand, "Impact");
  DrawSetFontSize(drawing_wand, pointsize * scale);
  DrawSetFontWeight(drawing_wand, 700);
  DrawSetTextInterlineSpacing(drawing_wand, -(pointsize / 3) * scale);
  DrawSetGravity(drawing_wand, NorthGravity);

  // Add a black outline to the text
  PixelSetColor(pixel_wand, "black");
  DrawSetStrokeWidth(drawing_wand, stroke_width * scale);
  DrawSetStrokeColor(drawing_wand, pixel_wand);

  // Turn on Anitalias
  DrawSetTextAntialias(drawing_wand, MagickTrue);

  // Draw the text
  DrawAnnotation(drawing_wand, 0, 0, (const unsigned char *)formatted_text);

  // Draw the image on the magick wand
  MagickDrawImage(wand, drawing_wand);

  // Write the image
  if (sink_image_path) {
    MagickWriteImage(wand, sink_image_path);
  } else {
    MagickWriteImageFile(wand, stdout);
  }

  // Clean up
  if(pixel_wand) pixel_wand = DestroyPixelWand(pixel_wand);
  if(drawing_wand) drawing_wand = DestroyDrawingWand(drawing_wand);
  if(wand) wand = DestroyMagickWand(wand);

  MagickWandTerminus();
}

int
main(int argc, char * const argv[])
{
  int option = 0;
  char *source_image_path = "logo:";
  char *sink_image_path = NULL;
  char *meme_text = "MEME";

  static struct option longopts[] = {
    { "help", no_argument, NULL, 'h'},
    { "source", required_argument, NULL, 'i' },
    { "sink", required_argument, NULL, 'o' },
    { "text", required_argument, NULL, 't' },
    {0, 0, 0, 0}
  };

  while ((option = getopt_long(argc, argv, "ht:i:o:", longopts, NULL)) != -1) {
    switch(option) {
    case 'h':
      printf("Usage: %s [OPTIONS]\n", argv[0]);
      printf("  -i, --source          Source image path\n");
      printf("  -o, --sink            Sink image path\n");
      printf("  -t, --text            Meme text\n");
      printf("  -h, --help            Print help\n");
      return 0;
    case 'i':
      source_image_path = optarg;
      break;
    case 'o':
      sink_image_path = optarg;
      break;
    case 't':
      meme_text = optarg;
      break;
    case '?':
      fprintf(stderr, "Try `%s --help` for more information.\n", argv[0]);
      return -2;
    }
  }

  generate(source_image_path, sink_image_path, meme_text);

  return 0;
}
