# sercer-post

This repo contains a RESTful service written in Haskell as requested by MetaLamp (https://rizzoma.com/topic/c27faf1cfa188c1120f59af4c35e6099/0_b_9n8n_8jl2l/).


## Intructions to set up and run

`Stack` was used for managing the packages, it is a fantastic tool that just works. You can find instructions on how to install [here](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

After installing `Stack`, clone this repo and `cd` into the project. In order to build the project, run
```
# download the compiler if needed
stack setup 

# build the project
stack build 
```

That's it! And then when you want to run the service at a port 3000:
```
stack exec server-post-exe
```


You can run the tests with:
```
stack test
```

## Project structure

- `app/Main.hs` - Prepares configs and handles and transfers control to the`Router`
- `haskell-checking-account.cabal` - `cabal` file automatically generated by `Stack`
- `package.yaml` - the file where we add all the dependencies and project metadata
- `stack.yaml` - `Stack` configuration
- `src` - contains all the code used by this service
    - `Router` - the main router, which will determine the essence of the request and transfer control to the appropriate controller
	- `FromRequst` - common functions used by other modules
    - `Controllers/` - controllers and routers of entities and additional endpoints
    - `Models/` - the entity models that our application uses (e.g. `User`, `Post`)
    - `Servises/` - Services required for the functioning of the application (e.g.`Logger, Config, DB)` 
      - `Impl/` - service implementations
- `test` - our specs, for testing some internal functions. 
- `sql` - sql database migration queries. Database is built and minimal initial seeding is ensured
- `bat`- batch files for easy testing of the application. Contains requests to the server passed by the `curl` utility for all endpoints of mseh entities (for Windows). In addition, `migration.bat` is a request for migration, a `test.bat` is a series of requests to fill the database for testing filtering and sorting of posts, `where-order.bat` - examples of tests for filtering and sorting of posts, `token.bat` - to refresh the token.
- `sh`- shell script files for easy testing of the application. Contains requests to the server passed by the `curl` utility for all endpoints of mseh entities (for Linux).
- `image` - folder from which you can upload photos to the database.


## Endpoints

Examples of all endpoints are provided as examples in batch files for Windows (folder 'bat') and files of shell script for Linux (folder 'sh'). Among them:

### Entity endpoints

Designed to perform basic operations on entities - creat, read, update, delete (CRUD)

### Endpoint 'token'

Endpoint of token renewal (at token expiration)

### Photo endpoints

Two endpoints for loading photos into the database from the request body (using the POST method) or from a file (PUT method).

### Viewing pictures

Links for getting avatars ('image') and photos ('photo').

### Endpoint 'publish'

Сreating or republishing an article based on a draft.

### Endpoint for filtering and  ordering posts

File 'where-order' (bat or sh) contains examples of using the endpoint for filtering and ordering posts.

### Endpoint 'migration'

Initializing the database.





 

