# Common build rules
#

# defaults
OUTPUT ?= bin
TARGET ?= $(OUTPUT)/Main.class
SOURCES ?= main.scala

all: $(TARGET)

test: all
	scala -classpath $(OUTPUT) $(notdir $(basename $(TARGET)))

clean:
	rm -rf $(OUTPUT)

$(TARGET): $(SOURCES) |$(OUTPUT)
	scalac -d $(OUTPUT) $(SOURCES)

$(OUTPUT):
	mkdir -p $@