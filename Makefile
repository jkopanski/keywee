GHC=stack ghc --
HSC=stack exec hsc2hs --
BUILD=build
LIBDIR=$(shell pwd)/lib
HSFLAGS=$(shell cat incopts.txt) -dynamic -fPIC -odir $(BUILD) -hidir $(BUILD) -i$(BUILD):cbits -Icbits
LDFLAGS=$(shell cat ldopts.txt) -dynamic -shared -L$(LIBDIR) -optl-Wl,-rpath,$(LIBDIR)
TARGET  = keywee.so
# order is important as Buffer depends on FFI etc.
OBJECTS = Types.o FFI.o Buffer.o Inbox.o FRP.o Monad.o Plugin.o keywee.o

.SUFFIXES = .hs .hsc
VPATH = plugin:plugin/FRP:plugin/WeeChat:cbits

incopts.txt ldopts.txt:
	stack exec perl opts.pl keywee

%.hs: %.hsc
	$(HSC) $< -o $@

%.o: %.hs incopts.txt
	$(GHC) $(HSFLAGS) -c $< -o $@

%.o: %.c
	$(GHC) $(HSFLAGS) -c $< -o $@

$(TARGET): $(OBJECTS)
	$(GHC) $(LDFLAGS) -no-hs-main $^ -o $@

all: $(TARGET)

clean:
	rm -rf $(BUILD)
	rm -rf $(LIBDIR)
	rm incopts.txt
	rm ldopts.txt
	rm $(OBJECTS)
	rm $(TARGET)

