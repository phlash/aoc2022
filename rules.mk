# Common build rules
#

# defaults
OUTPUT ?= bin
TARGET ?= $(OUTPUT)/Main.class
SOURCES ?= main.scala

all: $(TARGET)

test: all
	scala $(SCALA_OPTS) -classpath $(OUTPUT) $(notdir $(basename $(TARGET))) $(TEST_ARGS)

clean:
	rm -rf $(OUTPUT)

$(TARGET): $(SOURCES) |$(OUTPUT)
	scalac -d $(OUTPUT) $(SOURCES)

$(OUTPUT):
	mkdir -p $@
