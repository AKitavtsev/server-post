#!/bin/bash
bash <(curl -v POST http://localhost:3000/author/1.120210901202553ff034f3847c1d22f091dde7cde045264 -d "{\"author_id\":1, \"description\":\"Kitavtsev\"}")