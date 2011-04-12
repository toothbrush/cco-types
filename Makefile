CABAL-CONFIGURE-FLAGS := --user
CABAL-BUILD-FLAGS     :=

all : haskell

<<<<<<< HEAD
src/CCO/HM2SystemF/AG.hs : src/CCO/HM2SystemF/AG.ag src/CCO/HM2SystemF/AG/Base.ag
	uuagc -Hcfws --self -P src/CCO/HM2SystemF src/CCO/HM2SystemF/AG.ag

src/CCO/HM/AG.hs : src/CCO/HM/AG.ag src/CCO/HM/AG/Base.ag src/CCO/HM/AG/Infer.ag
	uuagc -Hdcfws --self -P src/CCO/HM src/CCO/HM/AG.ag

src/CCO/SystemF/AG.hs : src/CCO/SystemF/AG.ag src/CCO/SystemF/AG/Base.ag \
		src/CCO/SystemF/AG/Printing.ag
	uuagc -Hdcfws --self -P src/CCO/SystemF src/CCO/SystemF/AG.ag

clean :
	-rm src/CCO/SystemF/AG.hs
	-rm src/CCO/HM/AG.hs
	-rm src/CCO/HM2SystemF/AG.hs


haskell : src/CCO/HM/AG.hs src/CCO/SystemF/AG.hs src/CCO/HM2SystemF/AG.hs
	runhaskell Setup.lhs configure $(CABAL-CONFIGURE-FLAGS)
	runhaskell Setup.lhs build $(CABAL-BUILD-FLAGS)

documentation: latex-doc/main.tex
	pdflatex -output-directory=latex-doc latex-doc/main.tex 

clean : 
	-rm src/CCO/HM/AG.hs
	-rm src/CCO/SystemF/AG.hs
	cabal clean

.PHONY : haskell clean
