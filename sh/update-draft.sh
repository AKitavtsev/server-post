#!/bin/bash
bash <(curl -v -X PUT http://localhost:3000/draft/1.120210901202553ff034f3847c1d22f091dde7cde045264 -d "{\"id_draft\":1,\"newTitle\":\"it is Title\",\"newContent\":\"it is content\",\"newTags\":[1,3,10], \"newMainPhoto\":4, \"newCategory\":5,\"newOtherPhotos\":[1,3,10]}"
)