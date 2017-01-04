BASE_DIR=$(shell pwd)

CASK=cask

EMACS=emacs
EMACS_FLAGS=--batch
EMACS_BATCH_COMPILE_DIRECTORY=-l $(BASE_DIR)/q-byte-compile.el -f q-byte-compile-current-directory
EMACS_COMPILE_FILE=-f batch-byte-compile

days=day1.elc day2.elc day3.elc day4.elc day5.elc day6.elc day7.elc day8.elc day9.elc day10.elc day11.elc day12.elc day13.elc day14.elc day15.elc day16.elc day17.elc day18.elc day19.elc day20.elc day21.elc day22.elc day23.elc day24.elc day25.elc

compile: get_deps build_deps $(days)

%.elc: %.el
	@echo "==========Compiling: $^=========="
	$(EMACS) $(EMACS_FLAGS) $(EMACS_COMPILE_FILE) $^

all: clean compile

clean:
	@echo "==========Removing Predictive=========="
	[ -d "predictive" ] && cd predictive && make clean
	@echo "==========Removing Dash=========="
	[ -d "dash.el" ] && cd "dash.el" && git clean -f
	@echo "==========Cleaning Solutions=========="
	rm -rf *.elc

get_deps:
	@echo "==========Getting Predictive=========="
	([ -d "predictive" ] && cd predictive && git pull origin master) || (git clone http://www.dr-qubit.org/git/predictive.git)
	@echo "==========Getting Dash=========="
	([ -d "dash.el" ] && cd dash.el && git pull origin master) || (git clone https://github.com/magnars/dash.el.git)

build_deps:
	@echo "==========Building Dash=========="
	cd "dash.el" && $(CASK) build
	@echo "==========Building Predictive=========="
	[ -d "predictive" ] && cd predictive && make

