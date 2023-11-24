with ada.Strings.Maps; use ada.Strings.Maps;
with ada.Text_IO; use Ada.Text_IO;
with ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with ada.Numerics.Discrete_Random;
procedure Hangman is
    type wordRange is range 1..50;
    package rnd is new ada.Numerics.Discrete_Random(wordRange);
    use rnd;
    gen : rnd.Generator;
    displayBoard : array(1..12) of string(1..12);
    guesses : string(1..26);
    letterGuessed : character;
    answer : character;
    rndIndex : wordRange;
    word : string(1..20);
    wordLen : integer;
    userGuess : string(1..20);
    userGuessLen : integer;
    hidden : string(1..20);
    hiddenLen : integer;
    numGuesses : integer;
    numWrongGuesses : integer;
    numReplacedLetters : integer;
    count : integer;
    tempInt :integer;
    tempNum: wordRange;
    tempWord: string(1..20);
 

    wordArray : array(wordRange) of string(1..20) := (
        "gum                 ", "sin                 ", "for                 ", "cry                 ", "lug                 ", 
        "bye                 ", "fly                 ", "ugly                ", "each                ", "from                ", 
        "work                ", "talk                ", "with                ", "self                ", "pizza               ",
        "thing               ", "feign               ", "fiend               ", "elbow               ", "fault               ", 
        "dirty               ", "budget              ", "spirit              ", "quaint              ", "maiden              ", 
        "escort              ", "pickax              ", "example             ", "tension             ", "quinine             ",
        "kidney              ", "replica             ", "sleeper             ", "triangle            ", "kangaroo            ",
        "mahogany            ", "sergeant            ", "sequence            ", "moustache           ", "dangerous           ", 
        "scientist           ", "different           ", "quiescent           ", "magistrate          ", "erroneously         ",
        "loudspeaker         ", "phytotoxic          ", "matrimonial         ", "parasympathomimetic ", "thigmotropism       "
        );
begin

    -- Output intro message, before entering program loop
    put_line("The Game of Hangman");
    reset(gen);

    --randomize word order
    for i in wordRange loop
        tempNum := wordRange (i);
        tempWord := wordArray(tempNum);
        rndIndex := Random(gen);
        wordArray(tempNum):= wordArray(rndIndex);
    end loop;
    
    count := 1;
    --loop through words seeing if player can guess all 50 correctly 
    while count <= 50 loop
        tempNum := wordRange (count);
        tempInt := 1;
        wordLen := 0;
        word := wordArray(tempNum);

        --get length of the word
        for i in 1..20 loop
           if word(i) /= ' ' then
                wordLen := wordLen + 1;
            end if;
        end loop;
        
        --set default settings
        displayBoard(1) := (1..7 => 'X') & (1..5 => ' ');
        for i in 2..12 loop
            displayBoard(i)(1) := 'X';
            displayBoard(i)(2..12) := (2..12 => ' ');
        end loop;
        displayBoard(2)(7) := 'X';
        numGuesses := 0;
        numWrongGuesses := 0;
        guesses := (1..26 => ' ');
        hidden := (1..20 => '-');


        --get random word from wordArray
        tempNum := wordRange(count);
        word := wordArray(tempNum);
        hiddenLen := wordLen;

        --print hidden dashes 
        put_line(hidden(1..hiddenLen));

        while numWrongGuesses <= 10 loop

            -- Report letters that have been guessed
            put("The letters you have used so far are: ");
            for i in 1..numGuesses loop
                put(guesses(i));
                put(',');
            end loop;
            put_line(" ");

            put("Please guess a letter: ");
            get(letterGuessed);
            skip_line;

            if Is_In(letterGuessed, To_Set(guesses)) then
                put_line("You guessed that letter before.");
            else
                numGuesses := numGuesses + 1;
                guesses(numGuesses) := letterGuessed;

                numReplacedLetters := 0;
                for i in 1..wordLen loop
                    if word(i) = letterGuessed then
                        hidden(i) := letterGuessed;
                        numReplacedLetters := numReplacedLetters + 1;
                    end if;
                end loop;

               
                if numReplacedLetters = 0 then
                    numWrongGuesses := numWrongGuesses + 1;
                    put_line("Sorry, that letter isn't in the word.");

                    -- update display
                    case numWrongGuesses is
                        when 1 =>
                            put_line("First we draw a head.");
                            displayBoard(3)(6) := '-';
                            displayBoard(3)(7) := '-';
                            displayBoard(3)(8) := '-';
                            displayBoard(4)(5) := '(';
                            displayBoard(4)(6) := '.';
                            displayBoard(4)(8) := '.';
                            displayBoard(4)(9) := ')';
                            displayBoard(5)(6) := '-';
                            displayBoard(5)(7) := '-';
                            displayBoard(5)(8) := '-';
                        when 2 =>
                            put_line("Now we draw a body.");
                            displayBoard(6)(7) := 'X'; 
                            displayBoard(7)(7) := 'X'; 
                            displayBoard(8)(7) := 'X'; 
                            displayBoard(9)(7) := 'X'; 
                        when 3 =>
                            put_line("Next we draw an arm.");
                            displayBoard(4)(3) := '\'; 
                            displayBoard(5)(4) := '\'; 
                            displayBoard(6)(5) := '\'; 
                            displayBoard(7)(6) := '\'; 
                        when 4 =>
                            put_line("This time it's the other arm.");
                            displayBoard(4)(11) := '/';
                            displayBoard(5)(10) := '/';
                            displayBoard(6)(9) := '/';
                            displayBoard(7)(8) := '/';
                        when 5 =>
                            put_line("Now, Let's draw the right leg.");
                            displayBoard(10)(6) := '/';
                            displayBoard(11)(5) := '/';
                        when 6 =>
                            put_line("This time we draw the left leg.");
                            displayBoard(10)(8) := '\';
                            displayBoard(11)(9) := '\';
                        when 7 =>
                            put_line("Now we put up a hand.");
                            displayBoard(3)(11) := '\';
                        when 8 =>
                            put_line("Next the other hand.");
                            displayBoard(3)(3) := '/';
                        when 9 =>
                            put_line("Now we draw one foot");
                            displayBoard(12)(10) := '-';
                            displayBoard(12)(11) := '-';
                        when 10 =>
                            put_line("Here's the other foot.");
                            put_line("You're hung!!");
                            displayBoard(12)(4) := '-';
                            displayBoard(12)(3) := '-';
                        when others =>
                            null;
                    end case;

                    --print the display
                    for i in 1..12 loop
                        put_line(displayBoard(i));
                    end loop;
                elsif Is_In('-', To_Set(hidden(1..hiddenLen))) then
                    put_line(hidden(1..hiddenLen));
                    put_line("Please guess the word: ");
                    get_line(userGuess, userGuessLen);
                    if wordLen = userGuessLen and
                            word(1..wordLen) = userGuess(1..userGuessLen) then
                        put("Right! It took you ");
                        put(numGuesses);
                        put_line(" guesses.");
                        exit;
                    else
                        put_line("Wrong guess.");
                    end if;
                else
                    put_line("You found the word.");
                end if;
            end if;
        end loop;

        -- checks if user exceeds avalible attempts 
        if numWrongGuesses = 10 then
            put_line("Sorry, you lose. The word was " & word(1..wordLen));
        end if;

       
        put_line("Do you want another word? (Y/N)");
        get(answer);

        if answer /= 'Y' then
            exit;
        end if;
        count := count + 1;
    end loop;

    if count = 51 then
        put_line("You did all the words.");
    end if;
    put_line("It's been fun! Bye for now.");
    put_line("Ending...");

end Hangman;