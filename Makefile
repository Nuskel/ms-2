CXX		 	 = g++
CXXFLAGS = -std=c++20 -pedantic
OUTPUT   = ms

SRC=src/*.cpp
SRCS=$(wildcard $(SRC))
INCLUDE_DIR=include

all: clean $(OUTPUT)

$(OUTPUT): $(SRCS)
	@$(CXX) $(CXXFLAGS) -I$(INCLUDE_DIR) $(SRCS) main.cpp -o $@

clean:
	@rm -f $(OUTPUT)
