curl -v http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?tag=6
curl -v http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?tags_in=\[1,2\]
curl -v http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?tags_all=\[1,2\]
curl -v "http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?title=Paul%20M&tag=6"
curl -v "http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?text=Paul%20M&tag=1"
curl -v http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?created_gt=2021-08-03
curl -v http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?find=Paul

curl -v "http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?tags_in=\[1,2\]&title=Paul%20M&created_gt=2021-07-10&name=Bred&category=5&find=Pau&text=McC"

curl -v http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?order=\[photo,date,author,category\]

