package Ejercicios

case class MovieDatabase(   films: Map[Film.Id, Film],
                            users: Map[User.Id, User],
                            ratings: Map[(Film.Id, User.Id), Rating])

case class Film(   id: Film.Id,
                   title: String,
                   director: String,
                   genre: String,
                   year: Int,
                   country: String)

object Film{
    type Id = Int
}

case class User(   id: User.Id,
                   name: String,
                   registered: Int)

object User{
    type Id = Int
}

case class Rating( film: Film.Id,
                   user: User.Id,
                   score: Int)


object Data {
    val moviedb: MovieDatabase = MovieDatabase(
        films = Map(
            1 -> Film(1, "Blade Runner", "Ridley Scott", "Sci-Fi", 1982, "United States"),
            2 -> Film(2, "Amanece, que no es poco", "José Luis Cuerda", "Comedy", 1989, "Spain"),
            3 -> Film(3, "El milagro de P. Tinto", "Javier Fesser", "Comedy", 1998, "Spain"),
            4 -> Film(4, "Mars Attacks!", "Tim Burton", "Sci-Fi", 1996, "United States"),
            5 -> Film(5, "2001: A Space Odyssey", "Stanley Kubrick", "Sci-Fi", 1968, "United Kingdom"),
            6 -> Film(6, "El crack Cero", "José Luis Garci", "Film noir", 2019, "Spain"),
            7 -> Film(7, "El crack", "José Luis Garci", "Film noir", 1981, "Spain"),
            8 -> Film(8, "The Maltese Falcon", "John Huston", "Film noir", 1941, "United States"),
            9 -> Film(9, "Chinatown", "Roman Polanski", "Film noir", 1974, "United States"),
            10 -> Film(10, "Batman v. Superman: Dawn of Justice", "Zack Snyder", "Sci-Fi", 2016, "United States"),
            11 -> Film(11, "Dumb and Dumber", "Peter Farrelly", "Comedy", 1994, "United States")
        ),
        users = Map(
            1 -> User(1, "Juan", 1500),
            2 -> User(2, "Alf", 1555),
            3 -> User(3, "Lola", 1644),
            4 -> User(4, "Lola", 1655),
            5 -> User(5, "Dinu", 1622)),
        ratings = Map(
            (1,1) -> Rating(1,1,5),
            (1,2) -> Rating(1,2,1),
            (1,3) -> Rating(1,3,4),
            (1,4) -> Rating(1,4,3),
            (2,1) -> Rating(2,1,1),
            (2,4) -> Rating(2,4,1),
            (4,1) -> Rating(4,1,3),
            (5,4) -> Rating(5,4,2),
            (6,1) -> Rating(6,1,3),
            (6,2) -> Rating(6,2,3),
            (6,3) -> Rating(6,3,3),
            (7,1) -> Rating(7,1,2),
            (8,2) -> Rating(8,2,2),
            (9,1) -> Rating(9,1,1),
            (10,1) -> Rating(10,1,0),
            (10,3) -> Rating(10,3,0),
            (11,1) -> Rating(11,1,0),
            (11,2) -> Rating(11,2,1),
            (11,4) -> Rating(11,4,2)))
}

object BasicQueries{

    // Entities
    def films(mdb: MovieDatabase): List[Film] =
        mdb.films.values.toList

    def filmIds(mdb: MovieDatabase): List[Film.Id] =
        mdb.films.keys.toList

    def getFilm(id: Film.Id)(mdb: MovieDatabase): List[Film] =
        mdb.films.get(id).toList

    def userIds(mdb: MovieDatabase): List[User.Id] =
        mdb.users.keys.toList

    def getUser(id: User.Id)(mdb: MovieDatabase): List[User] =
        mdb.users.get(id).toList

    // 1-N relationships

    def films(dir: String)(mdb: MovieDatabase): List[Film.Id] =
        mdb.films.filter { case (_, film) => film.director.equals(dir) }.keys.toList

    // N-M relationships

    def ratings(mdb: MovieDatabase): List[Rating] =
        mdb.ratings.values.toList

    def userRatings(user: User.Id)(mdb: MovieDatabase): List[Rating] =
        mdb.ratings.filter { case (_, rating) => rating.user.equals(user) }.values.toList

    def filmRatings(film: Film.Id)(mdb: MovieDatabase): List[Rating] =
        mdb.ratings.filter { case (_, rating) => rating.film.equals(film) }.values.toList
}

import BasicQueries._

object Problem3_1 extends App {
    /* Implementation of BasicQueries */
}

object Problem3_2 extends App {

    def average(seq: List[Int]): Double =
        seq match {
            case _::_ => seq.foldLeft(0.0)(_+_) / seq.length
            case Nil => Double.NaN
        }
    println(
        "Part A) \n" +
        average(List()).isNaN + " shouldBe true\n" +
        average(List(1,2,3,4)) + " shouldBe 2.5\n" +
        average(List(1,1,1,1)) + " shouldBe 1.0\n" +
        average(List(1,5)) + " shouldBe 3.0"
    )

    def averageRating(film: Film.Id)(mdb: MovieDatabase): Double = {
//        average(filmRatings(film)(mdb).foldRight(List.empty[Int])((r: Rating, list:List[Int]) =>
//            list.appended(r.score)))
        average(filmRatings(film)(mdb).map(rating => rating.score))
    }

    println(
        "Part B) \n" +
        averageRating(1)(Data.moviedb) +" shouldBe 3.25\n" +
        averageRating(3)(Data.moviedb).isNaN +" shouldBe true\n" +
        averageRating(5)(Data.moviedb) +" shouldBe 2.0"
    )

    def wholeAverageRating(mdb: MovieDatabase): Double =
        average(ratings(mdb).foldRight(List.empty[Int])((r, list) => list.appended(r.score)))

    println(
        "Part C) \n" +
        wholeAverageRating(Data.moviedb) +" shouldBe 1.9473684210526316"
    )
}

object Problem3_3 extends App {

    def mostRated(mdb: MovieDatabase): List[(String, Int)] =
        films(mdb)
            .map(f => (f.title, filmRatings(f.id)(mdb).size))
            .sortWith((r1, r2) => r1._2 > r2._2)

    println(
        "Print A) Obtain a ranking of films, sorted by their number of ratings " +
            "in descending order\n" +
        mostRated(Data.moviedb) + "shouldBe" + List(
            ("Blade Runner", 4),
            ("El crack Cero", 3),
            ("Dumb and Dumber", 3),
            ("Batman v. Superman: Dawn of Justice", 2),
            ("Amanece, que no es poco", 2),
            ("2001: A Space Odyssey", 1),
            ("Chinatown", 1),
            ("El crack", 1),
            ("The Maltese Falcon", 1),
            ("Mars Attacks!", 1),
            ("El milagro de P. Tinto", 0)
        )
    )

    def topRated(mdb: MovieDatabase): List[(String, Double)] =
        ???
    
    println(
        "Part B) Obtain a ranking of films, sorted by their average score in ascending order.\n" +
        topRated(Data.moviedb).map{
            case (title, score) =>
                (title, if (score.isNaN) None else Some(score))
        } + " shouldBe " + List(
            ("Batman v. Superman: Dawn of Justice", Some(0.0)),
            ("Chinatown", Some(1.0)),
            ("Amanece, que no es poco", Some(1.0)),
            ("Dumb and Dumber", Some(1.0)),
            ("2001: A Space Odyssey", Some(2.0)),
            ("El crack", Some(2.0)),
            ("The Maltese Falcon", Some(2.0)),
            ("El crack Cero", Some(3.0)),
            ("Mars Attacks!", Some(3.0)),
            ("Blade Runner", Some(3.25)),
            ("El milagro de P. Tinto", None))
    )
}

object Problem3_4 extends App {
    def favourites(user: User.Id, n: Int)(mdb: MovieDatabase): List[Film.Id] =
        ???

    println(
        "Part A) Obtain the list of the n favourite films of a user (i.e. the " +
            "ones that the user gave them a highest score).\n" +
        favourites(1,3)(Data.moviedb) +" shouldBe List(1, 6, 4)\n" +
        favourites(3,2)(Data.moviedb) +" shouldBe List(1, 6)\n" +
        favourites(5,3)(Data.moviedb) +" shouldBe List()\n" +
        favourites(3,0)(Data.moviedb) +" shouldBe List()"
    )

    def ratingInfo(rating: Rating)(mdb: MovieDatabase): List[(String, String, Int)] =
        ???

    println(
        "Part B) Write a function that gives the name of the user, the title of the film, " +
            "and the score of a given rating.\n" +
        ratingInfo(Rating(1,1,3))(Data.moviedb) +" shouldBe " + List(("Juan", "Blade Runner", 3)) +
        ratingInfo(Rating(3,1,4))(Data.moviedb) +" shouldBe " + List(("Juan", "El milagro de P. Tinto", 4))
    )

    def favourites2(user: User.Id, n: Int)(mdb: MovieDatabase): List[(String, String, Int)] =
        ???

    println(
        favourites2(1,3)(Data.moviedb) + " shouldBe " +
            List(("Juan","Blade Runner",5),
                ("Juan","El crack Cero",3),
                ("Juan","Mars Attacks!",3)) +
        favourites2(3,2)(Data.moviedb) + " shouldBe " +
            List(("Lola","Blade Runner",4),
                ("Lola","El crack Cero",3)) +
        favourites2(5,3)(Data.moviedb) + " shouldBe " + List() +
        favourites2(3,0)(Data.moviedb) + " shouldBe " + List()
    )
}

object Problem3_5 extends App {
    def averageGenreRating(genre: String)(mdb: MovieDatabase): Double =
        ???

    println(
        "Part A) Obtain the average rating of all films that belongs to a given genre.\n" +
        averageGenreRating("Comedy")(Data.moviedb) + " shouldBe " + 1.0 +
        averageGenreRating("Film noir")(Data.moviedb) + " shouldBe " +2.3333333333333335 +
        averageGenreRating("Sci-Fi")(Data.moviedb) + " shouldBe " + 2.25
    )

    def averageGenreRatingFC(genre: String)(mdb: MovieDatabase): Double =
        ???

    println(
        "Part B) Use for-comprehensions to write the previous query.\n" +
        averageGenreRating("Comedy")(Data.moviedb) + " shouldBe " + 1.0 +
        averageGenreRating("Film noir")(Data.moviedb) + " shouldBe " +2.3333333333333335 +
        averageGenreRating("Sci-Fi")(Data.moviedb) + " shouldBe " + 2.25
    )

    def allGenreRatings(mdb: MovieDatabase): List[(String, Double)] =
        ???

    println(
        "Part c) Write a query that returns the average ratings of all genres" +
            "\n in the movie database. Particularly, the query must return a Map " +
            "\nwhose keys and values are the genres and their ratings, respectively.\n" +
        allGenreRatings(Data.moviedb) + " shouldBe " +
            List(("Sci-Fi",2.25),
                ("Film noir",2.3333333333333335),
                ("Comedy",1.0))
    )
}

