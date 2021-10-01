#!/bin/bash
bash <(curl -v POST http://localhost:3000/draft/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8 -d "{\"title\":\"My first new\", \"category\":3, \"tags\":[1,2,10],  \"t_content\":\"1111111111111111111\", \"mainPhoto\":2, \"otherPhotos\":[2,5]}"
)
