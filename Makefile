.PHONY: all clean deps

CC=cc -O2

CFILES=$(wildcard src/*.c)

OBJ=$(patsubst src/%,obj/%,$(patsubst %.c,%.o,$(CFILES)))
DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(OBJ)))

all: clox

clox: $(OBJ)
	$(CC) -o $@ $(OBJ)

-include $(DEP)

$(OBJ): obj/%.o: src/%.c | obj
	$(CC) -c $< -o $@

$(DEP): dep/%.d: src/%.c | dep
	$(CC) -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

dep:
	mkdir $@

obj:
	mkdir $@

clean: deps
	rm -f clox $(OBJ)

deps:
	rm -f $(DEP)

# vim: set noet,sw=8,tabstop=8
