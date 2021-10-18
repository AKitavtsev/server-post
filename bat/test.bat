curl -v http://localhost:3000/publish/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8/1
curl -v POST http://localhost:3000/author/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8 -d "{\"author_id\":2, \"description\":\"author2\"}"
curl -v POST http://localhost:3000/draft/2.0202111091126065c8b019cf55009a53f6ba5de31eec170 -d "{\"title\":\"John Lennon\", \"category\":6, \"tags\":[4,5],  \"text_content\":\"...Lennon was shot and killed in 1980\", \"main_photo\":2, \"other_photos\":[1]}"
curl -v http://localhost:3000/publish/2.0202111091126065c8b019cf55009a53f6ba5de31eec170/2
curl -v POST http://localhost:3000/draft/2.0202111091126065c8b019cf55009a53f6ba5de31eec170 -d "{\"title\":\"Paul McCartny\", \"category\":7, \"tags\":[4,6],  \"text_content\":\"...McCartney remain musically active\", \"main_photo\":3, \"other_photos\":[]}"
curl -v http://localhost:3000/publish/2.0202111091126065c8b019cf55009a53f6ba5de31eec170/3
curl -v POST http://localhost:3000/draft/2.0202111091126065c8b019cf55009a53f6ba5de31eec170 -d "{\"title\":\"George Harrison\", \"category\":8, \"tags\":[4],  \"text_content\":\"...Harrison died of lung cancer in 2001\", \"main_photo\":3, \"other_photos\":[1]}"
curl -v http://localhost:3000/publish/2.0202111091126065c8b019cf55009a53f6ba5de31eec170/4
curl -v POST http://localhost:3000/comment/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8 -d "{\"post_id\": 1, \"comment\":\"OK!!!!!!\"}"
curl -v POST http://localhost:3000/comment/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8 -d "{\"post_id\": 1, \"comment\":\"Go on, brother\"}"
curl -v POST http://localhost:3000/comment/2.0202111091126065c8b019cf55009a53f6ba5de31eec170 -d "{\"post_id\": 1, \"comment\":\"You are wrong brother\"}"


