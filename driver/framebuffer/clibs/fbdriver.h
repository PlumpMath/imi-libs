#ifndef FBDRIVER_H
#define FBDRIVER_H

#include <unistd.h>
#include <stdio.h>
#include <fcntl.h>
#include <stropts.h>
#include <linux/fb.h>
#include <sys/mman.h>

#include "bmap.h"

static int fbfd;

static const char* fbdev_init_return_information[] = {
    "framebuffer successfully initialized",
    "cannot open framebuffer device",
    "could not read fixed information",
    "could not read variable information",
    "failed to map framebuffer device to memory"
};

bmap* fbdev_open(char* fbfile);
void fbdev_close(bmap* fb);

#endif
