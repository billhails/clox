.PHONY: all clean deps profile

PROFILING=-pg
OPTIMIZING=-O2
DEBUGGING=-g

CC=cc $(OPTIMIZING)

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
	rm -f clox $(OBJ) callgrind.out.*

deps:
	rm -f $(DEP)

profile: clox
	rm -f callgrind.out.*
	valgrind --tool=callgrind ./clox ./samples/zoo.lox

# vim: noet,sw=8,tabstop=8
