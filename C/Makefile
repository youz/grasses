CC=gcc
CFLAGS=-Wall -Wno-strict-aliasing -std=gnu11 -g -I. -O2

ifeq ($(OS),Windows_NT)
	BINEXT = .exe
endif

TARGET=grass$(BINEXT)
OBJS=grass.o

$(TARGET): $(OBJS)
	$(CC) -o $@ $(OBJS) -lgc

all: $(TARGET)

clean:
	rm $(TARGET) $(TESTBIN) *.o
