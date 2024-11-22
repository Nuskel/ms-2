EXEC=ms

run: build/$(EXEC)
	./build/$(EXEC)

rebuild: build/Makefile
	make -C build clean $(EXEC)

cmake_make: cmake_build build/Makefile
	make -C build

cmake_build: CMakeLists.txt
	cmake -S . -B build