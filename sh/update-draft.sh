#!/bin/bash
bash <(curl -v -X PUT http://localhost:3000/draft/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8 -d "{\"id_draft\":1,\"new_title\":\"it is Title\",\"new_content\":\"it is content\",\"new_tags\":[1,3,10], \"new_main_photo\":4, \"new_category\":5,\"new_other_photos\":[1,3,10]}"
)