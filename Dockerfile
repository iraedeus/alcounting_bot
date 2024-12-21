FROM python:3.13-slim

WORKDIR /app

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
    sqlite3 && \
    rm -rf /var/lib/apt/lists/*


COPY pyproject.toml poetry.lock* /app/


RUN pip install --no-cache-dir poetry


RUN poetry install --no-root

COPY . /app

CMD ["poetry", "run", "python", "-m", "bot.__main__"]
