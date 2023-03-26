.PHONY: all clean deps

CC=cc

CFILES=$(wildcard src/*.c)

OBJ=$(patsubst src/%,obj/%,$(patsubst %.c,%.o,$(CFILES)))
DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(OBJ)))

all: clox

clox: $(OBJ)
	$(CC) -o $@ $(OBJ)

-include $(DEP)

$(OBJ): obj/%.o: src/%.c
	$(CC) -c $< -o $@

$(DEP): dep/%.d: src/%.c
	$(CC) -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

clean: deps
	rm -f clox $(OBJ)

deps:
	rm -f $(DEP)
