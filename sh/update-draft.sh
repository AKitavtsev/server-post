#!/bin/bash
bash <(curl -v -X PUT http://localhost:3000/draft/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8 -d "{\"id_draft\":1,\"newTitle\":\"it is Title\",\"newContent\":\"it is content\",\"newTags\":[1,3,10], \"newMainPhoto\":4, \"newCategory\":5,\"newOtherPhotos\":[1,3,10]}"
)