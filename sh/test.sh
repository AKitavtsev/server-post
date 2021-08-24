#!/bin/bash
bash <(curl -v http://localhost:3000/publish/1.120210901202553ff034f3847c1d22f091dde7cde045264/1)
bash <(curl -v POST http://localhost:3000/author/1.120210901202553ff034f3847c1d22f091dde7cde045264 -d "{\"author_id\":2, \"description\":\"author2\"}")
bash <(curl -v POST http://localhost:3000/draft/2.0202111091126065c8b019cf55009a53f6ba5de31eec170 -d "{\"title\":\"John Lennon\", \"category\":6, \"tags\":[4,5],  \"t_content\":\"...Lennon was shot and killed in 1980\", \"mainPhoto\":2, \"otherPhotos\":[1]}")
bash <(curl -v http://localhost:3000/publish/2.0202111091126065c8b019cf55009a53f6ba5de31eec170/2)
bash <(curl -v POST http://localhost:3000/draft/2.0202111091126065c8b019cf55009a53f6ba5de31eec170 -d "{\"title\":\"Paul McCartny\", \"category\":7, \"tags\":[4,6],  \"t_content\":\"...McCartney remain musically active\", \"mainPhoto\":3, \"otherPhotos\":[]}")
bash <(curl -v http://localhost:3000/publish/2.0202111091126065c8b019cf55009a53f6ba5de31eec170/3)
bash <(curl -v POST http://localhost:3000/draft/2.0202111091126065c8b019cf55009a53f6ba5de31eec170 -d "{\"title\":\"George Harrison\", \"category\":8, \"tags\":[4],  \"t_content\":\"...Harrison died of lung cancer in 2001\", \"mainPhoto\":3, \"otherPhotos\":[1]}")
bash <(curl -v http://localhost:3000/publish/2.0202111091126065c8b019cf55009a53f6ba5de31eec170/4)
bash <(curl -v POST http://localhost:3000/comment/1.120210901202553ff034f3847c1d22f091dde7cde045264 -d "{\"post_id\": 1, \"comment\":\"OK!!!!!!\"}")
bash <(curl -v POST http://localhost:3000/comment/1.120210901202553ff034f3847c1d22f091dde7cde045264 -d "{\"post_id\": 1, \"comment\":\"Go on, brother\"}")
bash <(curl -v POST http://localhost:3000/comment/2.0202111091126065c8b019cf55009a53f6ba5de31eec170 -d "{\"post_id\": 1, \"comment\":\"You are wrong brother\"}")


