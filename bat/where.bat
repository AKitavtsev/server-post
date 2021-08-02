curl -v http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?tag=6
curl -v http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?tags_in=\[1,2\]
curl -v http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?tags_all=\[1,2\]
curl -v "http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?title=Paul%20M&tag=6"
curl -v "http://localhost:3000/posts/1.120210901202553ff034f3847c1d22f091dde7cde045264?text=Paul%20M&tag=1"