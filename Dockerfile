FROM r-base

WORKDIR /app

ADD bootstrap.sh .
ADD packages.R .

RUN chmod +x bootstrap.sh
RUN ./bootstrap.sh

COPY . .

ENV PORT=8080

CMD ["Rscript", "DEMO.R"]
