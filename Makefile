.PHONY: all clean

CC=cc

CFILES=$(wildcard src/*.c)

OBJ=$(patsubst src/%,obj/%,$(patsubst %.c,%.o,$(CFILES)))
DEP=$(patsubst obj/%,dep/%,$(patsubst %.o,%.d,$(OBJ)))

all: clox
	./clox

clox: $(OBJ)
	$(CC) -o $@ $(OBJ)

-include $(DEP)

$(OBJ): obj/%.o: src/%.c
	$(CC) -c $< -o $@

$(DEP): dep/%.d: src/%.c
	$(CC) -MM -MT $(patsubst dep/%,obj/%,$(patsubst %.d,%.o,$@)) -o $@ $<

clean:
	rm clox $(OBJ) $(DEP)