TEST_SRCS = $(wildcard *.c)
BUILD_DIR = ../..
CCFLAGS = -lsbcl -lquilc -L$(BUILD_DIR) -I$(BUILD_DIR)

OS:=$(shell uname -s)
ifeq ($(OS), Darwin)
	CCFLAGS += -pagezero_size 0x100000
endif

.PHONY: all clean

all: $(TEST_SRCS:.c=)

%: %.c
	gcc $< -o $@ $(CCFLAGS)

clean:
	rm -f $(TEST_SRCS:.c=)
