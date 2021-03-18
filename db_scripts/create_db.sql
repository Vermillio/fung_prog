DROP TABLE IF EXISTS students;
DROP TABLE IF EXISTS teachers;
DROP TABLE IF EXISTS tasks;
DROP TABLE IF EXISTS groups_;
DROP TABLE IF EXISTS themes;
DROP TABLE IF EXISTS resources;
DROP TABLE IF EXISTS questions;
DROP TABLE IF EXISTS answers;

CREATE TABLE IF NOT EXISTS students
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(20),
    group_id VARCHAR(20) REFERENCES groups_ (id)
);

CREATE TABLE IF NOT EXISTS groups_
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(20)
);

CREATE TABLE IF NOT EXISTS teachers
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(20),
    working_hours_start INT,
    working_hours_end INT
);

CREATE TABLE IF NOT EXISTS tasks
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(120),
    content VARCHAR(1000),
    student_id INT REFERENCES students (id),
    theme_id INT REFERENCES themes (id),
    status VARCHAR(120)
);

CREATE TABLE IF NOT EXISTS themes
(
    id SERIAL PRIMARY KEY,
    name VARCHAR(120),
    content VARCHAR(120)
);

CREATE TABLE IF NOT EXISTS resources
(
    id SERIAL PRIMARY KEY,
    link VARCHAR(200),
    theme_id INT REFERENCES themes (id)
);

CREATE TABLE IF NOT EXISTS question
(
    id SERIAL PRIMARY KEY,
    student_id INT REFERENCES teachers (id),
    content VARCHAR(500)
);

CREATE TABLE IF NOT EXISTS answer
(
    id SERIAL PRIMARY KEY,
    teacher_id INT REFERENCES teachers (id),
    question_id INT REFERENCES questions (id),
    content VARCHAR(500)
);

