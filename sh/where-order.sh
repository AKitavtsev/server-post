#!/bin/bash
bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8?title=%%Paul%%20M%%")

bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8?tag=6")

bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8?tags_in=\[1,2\]")

bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8?tags_all=\[1,2\]")


bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8?text=%%Paul%%&tag=1")

bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8?name=Bred&tag=6")

bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8?created_gt=2021-08-03&page=2")

bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8?page=2")

bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8?created_lt=2021-08-03")


bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8?find=%%Paul%%&page=2")

bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8?tags_in=\[1,2\]&title=%%Paul%%&created_gt=2021-07-10&name=Bred&category=5&find=%%Pau%%&text=%%McC%%")

bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8?order=\[photo,date,author,category\]"
bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8?order=\[photo,date,author,category\]&page=2")


bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8/1")
bash <(curl -v "http://localhost:3000/posts/1.12021122211403534ebda4d9b00f8ac99207b459fc4d1f8/1?page=2")
