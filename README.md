FutuHours-next
=========

Application for updating and editing employee hours, all new and re-written in Elm and elm-ui.

Currently in Beta and deployed at [betahours](https://betahours.app.futurice.com) (internal link only).

**Requirements** 

- npm
- [create-elm-app](https://github.com/halfzebra/create-elm-app)
- Docker (for deploying)
- [Futuswarm](https://github.com/futurice/futuswarm) (for deploying)
  - Futurice employees only use [Appswarm](https://futuswarm-mainpage.app.futurice.com/)

**Development**

To start the development environment, do:

```
$ elm-app start
```

**Deployment**

To deploy:

``` 
$ docker build -t futurice/futuhours-next:$(git log --pretty=format:'%h' -n 1) .
$ appswarm image:push -i futurice/futuhours-next -t $(git log --pretty=format:'%h' -n 1)
$ appswarm app:deploy -i futurice/futuhours-next -t $(git log --pretty=format:'%h' -n 1) -n betahours
```

Alternately, a simple bash script is provided to save you typing:

```
./deploy.sh
```


**License**

Copyright 2019 by Futurice Oy

[Apache License 2.0](LICENSE)
