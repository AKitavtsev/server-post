#!/bin/bash
bash <(curl -v POST http://localhost:3000/draft/1.120210901202553ff034f3847c1d22f091dde7cde045264 -d "{\"title\":\"My first new\", \"category\":3, \"tags\":[1,2,10],  \"t_content\":\"1111111111111111111\", \"mainPhoto\":2, \"otherPhotos\":[2,5]}"
)
