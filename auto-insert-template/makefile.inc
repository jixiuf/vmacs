# -*- coding:utf-8 -*-
INCLUDES = -I ./inc ## list of more includes omitted for brevity
.PHONY: check-syntax
check-syntax:
	gcc -o nul  -Wall -Wextra  -pedantic -fsyntax-only -Wno-variadic-macros -std=c99 $(INCLUDES) -S ${CHK_SOURCES}
# 	g++ -o nul  -Wall -Wextra  -pedantic -fsyntax-only -Wno-variadic-macros -std=c99 $(INCLUDES) -S ${CHK_SOURCES}
