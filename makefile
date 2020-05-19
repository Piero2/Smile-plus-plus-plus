#
# Rules for compiling and linking the typechecker/evaluator
#
# (Copied and adapted from the lab 2 solutions)
#
# Type
#   make         to rebuild the executable files
#   make clean   to remove all intermediate and temporary files
#

# Files that need to be generated from other files
DEPEND += TokensCW.hs GrammarCW.hs myinterpreter.hs

# When "make" is invoked with no arguments, we build an executable
#  after building everything that it depends on
all: $(DEPEND) myinterpreter

# Build an executable for our interpreter
myinterpreter: $(DEPEND) myinterpreter.hs
	ghc myinterpreter.hs

# Generate files from a parser definition file
GrammarCW.hs : GrammarCW.y
	@rm -f GrammarCW.hs
	happy GrammarCW.y
	@chmod -w GrammarCW.hs

# Generate files from a lexer definition file
TokensCW.hs : TokensCW.x
	@rm -f TokensCW.hs
	alex TokensCW.x
	@chmod -w TokensCW.hs

# Clean up the directory
clean::
	rm -rf TokensCW.hs GrammarCW.hs *.hi *.o *.info
