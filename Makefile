.PHONY: all clean

CFILES=$(wildcard src/*.c)
HFILES=$(wildcard src/*.h)

OBJ=$(patsubst src/%,obj/%,$(patsubst %.c,%.o,$(CFILES)))

all: clox

clox: $(OBJ)
	cc -o $@ $(OBJ)

$(OBJ): $(HFILES)

$(OBJ): obj/%.o: src/%.c
	cc -c $< -o $@

clean:
	rm clox $(OBJ)