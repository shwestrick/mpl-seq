DEFAULT_FLAGS?=-default-type int64 -default-type word64

all: main.mpl-cd

phony:

main.%: phony
	@echo MPL=`cat config/$*`
	`cat config/$*` $(DEFAULT_FLAGS) -output $@ main.mlb
