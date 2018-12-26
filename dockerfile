FROM node:alpine

RUN mkdir /usr/local/byappt

WORKDIR /usr/local/byappt

COPY package.json .
RUN [ "/usr/local/bin/npm", "install" ]

COPY . .
RUN [ "/usr/local/byappt/build.sh" ]