CURDIR := $(shell pwd)

PROJECT = memebot

C_SRC_OUTPUT ?= $(CURDIR)/../priv/$(PROJECT)

CC=cc
CFLAGS=`pkg-config --cflags MagickWand` -g -Wall -pg
LDFLAGS=`pkg-config --libs MagickWand`
OBJECTS=$(PROJECT).c

.PHONY: clean

$(C_SRC_OUTPUT): $(OBJECTS)
	$(CC) -o $@ $< $(CFLAGS) $(LDFLAGS)

clean:
	@rm -f $(C_SRC_OUTPUT)
