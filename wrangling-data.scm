;; CSC-151 (Fall-2022)
;; Lab: Wrangling Data
;; Authors: Seunghyeon(Hyeon) Kim, Charles Wade
;; Date: 30-09-2022
;; Acknowledgements:
;;   ACKNOWLEDGEMENTS HERE
(define passwords (list "jjmsjjms" "J0n0th0n" "Lausebengel" "Poeldijk1" "8fmabg7k" "Komigjen1" "sunyingjie" "Leshen7" "Our2boys" "15MAI1984" "ANNAEMILIA" "nmstnmst" "Avidex9" "Ma12ma12" "faciofacio" "OOSTMEERS" "Ez24g3t" "harleyrae1" "PODKOREN" "fyrkilde" "bierbrouwen" "puigpuig" "Xuehao" "hphshphs" "Mom2three" "tushartushar" "Tomgerry1" "breaksbreaks" "20FA89" "M1ll1ona1re" "Chingerel0" "Bigtoby1" "class a" "280275s10810" "Houghto1" "Guldsko" "Colepaige1" "choco24nuts" "not4everyone" "Ordalia3" "Skyisblue" "P1lchard" "amparopozi" "Jugadorn12" "198561198561" "soulmail2" "890-iop[" "Golftime" "akkieakkie" "Rebowin2" "Treysmom1" "Zuidveen" "fine20fine" "Hotjeff" "Cirkushest0" "antoonantoon" "Peps1c0la" "Michugo1" "Songyuan" "Woodborough" "Qaz1xsw2" "zhenshanmei" "Ryandean1" "Rugermini14" "Princesahermosa1" "Edgarleon" "Alyssaaj1" "John1son" "J6i2m513" "Jakerobert3" "TANGASLIP" "clydexclydex" "mdkgmdkg" "rosexn113" "LAMLOOMA" "Op10mist" "G00dbear" "B33rm3" "dumbardumbar" "wablieF" "Kaupunki" "Kissoflove" "Jencole1" "noydbnoydb" "Averybaby" "anitkram" "Lolodog1" "Dol12phin" "rosiepink" "Adworks1" "Corv3tte" "jieaixinyi" "Nokia6500slide" "ppdsppds" "Jkwjkw5" "nastybastard" "bessy_2000" "VEERANJANEYA" "l1vl1v" "dead2night" "all4asoul" "Letmein4now" "h870323c" "menckmenck" "IsmmIsmm" "Fr33ksh0w" "Minnwild1" "FerFer" "Brygadzistka" "usdfusdf" "Sp0rtster" "Esathigh1" "hayathnagar" "sllyprsn" "Teamhardy" "gjsgjsgjsgjs" "Shtmlf!" "onarafittab" "Seanmcrae" "6rzbfyx" "MARCORAMOS" "footba!!" "Arklite6" "alanthecat" "zedernholz1" "Thruhim1" "viatabatefilmul" "Leggies4" "Catchhim" "Stevejade4" "shallyqian" "Go4number1" "Shoey2" "Ca4350799" "Hels1nki" "Ca55anova" "Work4kids" "Shanesmith" "BERZOSA" "Fish4it" "Tominello" "snow58man" "feddefedde" "Gobadgers2" "Josiahd2" "KLUKKLUK" "pop11corn" "B4its2l8" "verghereto" "lwesthau" "mejormejor" "Dannstadt" "rich6ard" "Kidsvolley1" "Cruiser4x4" "Spablauw2" "Tallhat1" "Naridi5" "phildecker" "Babyelvis" "Big12boy" "Luvcows1" "Billdeb1" "Xdr56yhn" "Lovedean1" "LINKALOT" "R0adrash" "7inzexue" "GRUSHEVSKY" "frogdancing" "markalo04" "WIN2003SERVER" "rathmalana" "EEW102995" "Cesamet1" "Marialouise1" "Nickelgoat66" "eexffrhb" "Loperhet1" "kurrikurri" "signdude" "T1r2o3y4" "Testastretta1" "pingithree" "deo7serve" "Idrhba9t" "Gossipgirl" "85x8nug8" "RINKURINKU" "ihtmihtm" "Jumbis3" "cacpcacp" "Ashlyp4" "Lulupie" "K1w1land" "Millerhighlife" "Shinig5" "pruteprute" "Putujemo3" "Studbagel1" "henoshenos" "zhengbo645917" "Bobbybowden1" "pariseparise" "rhyz2pare" "D0g5hit" "tanyifan" "scrubsuit" "Enguerran" "eameenehc" "nederename" "Melissajade3" "ginanu125" "linmolinmo" "Frihjul" "ctmpk4td" "L0rdship" "nielasus572754sh" "chuanshan" "awes0m3" "Mrminky1" "Ashliej1" "Educat10n" "bhuvanbhuvan" "W3bsites" "Abethebabe1" "513hvandmit" "bondsrus" "krcdkrcd" "irq7pozitron" "Krafle76" "rekabnala" "All4them" "kaszyca2" "Kaylaliz3" "Kimballl1" "zheshimimi" "jrzygrl" "Delaneybug7" "40lei401" "ih8laura" "Vetemune!" "Slatem3" "sejasiap" "Livefr33" "k101875k" "F0rmu1a" "xxrahmixx" "sandepsandep" "Katoes1" "Katiekitty" "Gayatrii1" "This1again" "Angu1lla" "FINALFELIZ" "Ladycocca1" "w1nbl00d" "Niecie" "Safedriver1" "p@$sw0rd" "Pulpfict1" "2oauiodr" "Crossbike7" "Lugan0" "Paraloft" "Rdxcft67" "P4ss10n" "CRAZ66Y" "Newstreet1" "ymenemcm" "Huntingman3" "bjerager1" "marjan!!!" "udahlupA" "Vontee1" "SVANEN11" "stanleythegreat" "my_rolla" "l822rot" "ifmxifmx" "zuddzudd" "Golfballen1" "brianshima" "rainartcanada" "Boattrap1" "Poderos0" "Cocobarbie" "Na2s2o3" "0ldfr1ends" "Tammemets" "Abbiegurl" "Dionesia1" "try2hide" "lizeyang." "Hypn0s1s" "y6@y@6cv" "161184/lokura" "Pwaosrsd2" "Bullybum1" "Feb232003" "Khaulid01" "rampurhat" "jam4fonte" "Greatleader1" "lauraantonelli" "Kipperdog" "nicolesese13" "Soloy0" "The2boys" "Itiswhatitis8" "harrishill" "xinxin72302" "Myhaley1" "striirts" "siprosipro" "zhuguoqiang" "Yindain5" "Drewnick" "Puddicombe" "Telpaneca1" "Schigulski" "Azwildcats1" "Marissad1" "gridfoot46" "Homefinder1" "PINCESA1" "ellejjelle" "amlesamles" "ewbtciasT" "luckyjava1" "want4more" "hairamyerac" "pzofmind1" "Pa33w8rd" "xangogal1" "iaiyuanyi" "kkk8c44" "Howardstern" "Mariensztat8" "J1ngj1ng" "Bignam1" "Lackschuh" "Kr1ss1e" "Tylerwolf" "Ladyh1" "Mich12igan" "NIDHISHARMA" "wienlinz" "Rileyquinn" "Jellyf1sh" "jiujitsu13!@" "INDRAPRASTHA" "anerianeri" "Verbalkent" "48penel96" "suunsuun" "brianshouse" "Jonmarc1" "miaouhmiaouh" "Ninonico" "Scamarcio" "lainnlainn" "Wutang4life" "Blue2star" "czeremosz" "Janepaul1" "Leplubo6" "wzmlbjdyfq" "Gr8cie" "~~jmle84~~*" "Nicoti1" "zadehzadeh" "soy37983798" "godreppiz" "straatkind" "Vilmoskorte" "Samyboy1" "aryanvaid" "dvdadvda" "Momoftwo" "My2babas" "Kakaramea" "hechuang1969" "tkdgurvkdl" "shanhujiao" "dave1330dave" "Tonirae4" "Loveava1" "ledrurolin" "nioukniouk" "Dogsrun" "n1cklaus" "kris*10" "gxxw100k" "khoksamrong" "Marlona1" "Miyochan1" "schapenhok" "Treadstones9" "petenlentz" "Iamadork3" "Macpup1" "shayruby" "nishiyashiki" "Faronesque1" "utsautsa" "Flygold3" "lsjvdpd" "S1nt3rklaas" "Meghdoot" "Sillepigen" "livenote1" "Milo1cat" "Vanderwerp2" "chenyuping" "heinersdorf" "SPOMRAC" "Yhhaabor" "Albertjr1" "My6love" "Tazlady1" "Hopeglory" "Laugh4me" "The1cure" "Ycgu8101" "cloadms1" "Ilovejtb3" "Boofuls1" "Trickout2" "L3opard" "cpaabv01" "D4t4l1nk" "vlezenbeek" "Terijean1" "kapownik1" "dlfxn780508" "Bakerdog1" "Plenitudine" "orgenoyar" "zreikfatima" "kimhwansung" "egilsegils" "brolobrolo" "Trojaczek" "Yodapower1" "lewaplewap" "doliromi28" "Peppersauce1" "doyelchanda" "Mongee1" "Kingspan" "Eulental" "gnuergnuer" "Cleo5cleo" "huasirlon" "e2822mail" "Awariki1" "LAPAROSCOPY" "Dawnski1" "mjc1ussmonitor" "HERZEEUW" "bobyybob" "Yanibah7" "Rainy2day" "Belohorizonte" "morinimorini" "cachaceiro" "Dendeng1" "Crizecrize1" "Warriorwoman7" "Piperl1" "natureisbest1" "slwslwslw5" "Indianjoe4" "ooottthhh" "isawlate" "mnmbrsnd" "rca6l6" "Whitneylynn1" "kuftakufta" "L1thuania" "Jovigirl1" "1mclxviii" "Jelifish1" "Foomanchu2" "al2o3sio2" "FULGUERAS" "gudwn99" "Kerstinj7" "brsogr01" "nepbjP" "and38rea" "Mer1dian" "Tommymuis" "fri28jun" "RynoRyno" "alexishaley" "julio1julio" "Redstr1pe" "Ob1knob"))

(define text "“Well, Prince, so Genoa and Lucca are now just family estates of the\nBuonapartes. But I warn you, if you don’t tell me that this means war,\nif you still try to defend the infamies and horrors perpetrated by that\nAntichrist—I really believe he is Antichrist—I will have nothing\nmore to do with you and you are no longer my friend, no longer my\n‘faithful slave,’ as you call yourself! But how do you do? I see I\nhave frightened you—sit down and tell me all the news.”\n\nIt was in July, 1805, and the speaker was the well-known Anna Pávlovna\nSchérer, maid of honor and favorite of the Empress Márya Fëdorovna.\nWith these words she greeted Prince Vasíli Kurágin, a man of high\nrank and importance, who was the first to arrive at her reception. Anna\nPávlovna had had a cough for some days. She was, as she said, suffering\nfrom la grippe; grippe being then a new word in St. Petersburg, used\nonly by the elite.\n\nAll her invitations without exception, written in French, and delivered\nby a scarlet-liveried footman that morning, ran as follows:\n\n“If you have nothing better to do, Count (or Prince), and if the\nprospect of spending an evening with a poor invalid is not too terrible,\nI shall be very charmed to see you tonight between 7 and 10—Annette\nSchérer.”\n\n“Heavens! what a virulent attack!” replied the prince, not in the\nleast disconcerted by this reception. He had just entered, wearing an\nembroidered court uniform, knee breeches, and shoes, and had stars on\nhis breast and a serene expression on his flat face. He spoke in that\nrefined French in which our grandfathers not only spoke but thought, and\nwith the gentle, patronizing intonation natural to a man of importance\nwho had grown old in society and at court. He went up to Anna Pávlovna,\nkissed her hand, presenting to her his bald, scented, and shining head,\nand complacently seated himself on the sofa.\n\n“First of all, dear friend, tell me how you are. Set your friend’s\nmind at rest,” said he without altering his tone, beneath the\npoliteness and affected sympathy of which indifference and even irony\ncould be discerned.\n\n“Can one be well while suffering morally? Can one be calm in times\nlike these if one has any feeling?” said Anna Pávlovna. “You are\nstaying the whole evening, I hope?”\n\n“And the fete at the English ambassador’s? Today is Wednesday. I\nmust put in an appearance there,” said the prince. “My daughter is\ncoming for me to take me there.”\n\n“I thought today’s fete had been canceled. I confess all these\nfestivities and fireworks are becoming wearisome.”\n\n“If they had known that you wished it, the entertainment would have\nbeen put off,” said the prince, who, like a wound-up clock, by force\nof habit said things he did not even wish to be believed.\n\n“Don’t tease! Well, and what has been decided about Novosíltsev’s\ndispatch? You know everything.”\n\n“What can one say about it?” replied the prince in a cold, listless\ntone. “What has been decided? They have decided that Buonaparte has\nburnt his boats, and I believe that we are ready to burn ours.”\n\nPrince Vasíli always spoke languidly, like an actor repeating a stale\npart. Anna Pávlovna Schérer on the contrary, despite her forty years,\noverflowed with animation and impulsiveness. To be an enthusiast had\nbecome her social vocation and, sometimes even when she did not\nfeel like it, she became enthusiastic in order not to disappoint the\nexpectations of those who knew her. The subdued smile which, though it\ndid not suit her faded features, always played round her lips expressed,\nas in a spoiled child, a continual consciousness of her charming defect,\nwhich she neither wished, nor could, nor considered it necessary, to\ncorrect.\n\nIn the midst of a conversation on political matters Anna Pávlovna burst\nout:\n\n“Oh, don’t speak to me of Austria. Perhaps I don’t understand\nthings, but Austria never has wished, and does not wish, for war. She\nis betraying us! Russia alone must save Europe. Our gracious sovereign\nrecognizes his high vocation and will be true to it. That is the one\nthing I have faith in! Our good and wonderful sovereign has to perform\nthe noblest role on earth, and he is so virtuous and noble that God will\nnot forsake him. He will fulfill his vocation and crush the hydra of\nrevolution, which has become more terrible than ever in the person of\nthis murderer and villain! We alone must avenge the blood of the just\none.... Whom, I ask you, can we rely on?... England with her commercial\nspirit will not and cannot understand the Emperor Alexander’s\nloftiness of soul. She has refused to evacuate Malta. She wanted to\nfind, and still seeks, some secret motive in our actions. What answer\ndid Novosíltsev get? None. The English have not understood and cannot\nunderstand the self-abnegation of our Emperor who wants nothing for\nhimself, but only desires the good of mankind. And what have they\npromised? Nothing! And what little they have promised they will not\nperform! Prussia has always declared that Buonaparte is invincible, and\nthat all Europe is powerless before him.... And I don’t believe a\nword that Hardenburg says, or Haugwitz either. This famous Prussian\nneutrality is just a trap. I have faith only in God and the lofty\ndestiny of our adored monarch. He will save Europe!”\n\nShe suddenly paused, smiling at her own impetuosity.\n\n“I think,” said the prince with a smile, “that if you had been\nsent instead of our dear Wintzingerode you would have captured the King\nof Prussia’s consent by assault. You are so eloquent. Will you give me\na cup of tea?”\n\n“In a moment. À propos,” she added, becoming calm again, “I am\nexpecting two very interesting men tonight, le Vicomte de Mortemart, who\nis connected with the Montmorencys through the Rohans, one of the best\nFrench families. He is one of the genuine émigrés, the good ones. And\nalso the Abbé Morio. Do you know that profound thinker? He has been\nreceived by the Emperor. Had you heard?”\n\n“I shall be delighted to meet them,” said the prince. “But\ntell me,” he added with studied carelessness as if it had only just\noccurred to him, though the question he was about to ask was the chief\nmotive of his visit, “is it true that the Dowager Empress wants\nBaron Funke to be appointed first secretary at Vienna? The baron by all\naccounts is a poor creature.”\n\nPrince Vasíli wished to obtain this post for his son, but others were\ntrying through the Dowager Empress Márya Fëdorovna to secure it for\nthe baron.\n\nAnna Pávlovna almost closed her eyes to indicate that neither she nor\nanyone else had a right to criticize what the Empress desired or was\npleased with.\n\n“Baron Funke has been recommended to the Dowager Empress by her\nsister,” was all she said, in a dry and mournful tone.\n\nAs she named the Empress, Anna Pávlovna’s face suddenly assumed an\nexpression of profound and sincere devotion and respect mingled with\nsadness, and this occurred every time she mentioned her illustrious\npatroness. She added that Her Majesty had deigned to show Baron Funke\nbeaucoup d’estime, and again her face clouded over with sadness.\n\nThe prince was silent and looked indifferent. But, with the womanly and\ncourtierlike quickness and tact habitual to her, Anna Pávlovna\nwished both to rebuke him (for daring to speak as he had done of a man\nrecommended to the Empress) and at the same time to console him, so she\nsaid:\n\n“Now about your family. Do you know that since your daughter came\nout everyone has been enraptured by her? They say she is amazingly\nbeautiful.”\n\nThe prince bowed to signify his respect and gratitude.\n\n“I often think,” she continued after a short pause, drawing nearer\nto the prince and smiling amiably at him as if to show that political\nand social topics were ended and the time had come for intimate\nconversation—“I often think how unfairly sometimes the joys of life\nare distributed. Why has fate given you two such splendid children?\n")

#|
Passwords Questions:

1. What percent of passwords contain at least 1 number?
2. Average length of password?
3. How many passwords start with capital letter?

Text questions:

1. How many lines are there?
2. How many words are in it?
|#

#|What percent of passwords contain at least 1 number?|#
; (contains-number? string): boolean?
; string: string?
; It returns the boolean of whether the string contains any numeric char.
(define contains-number?
    (lambda (string)
        (let* ([string-list (string->list string)]
               [filtered-list (filter char-numeric? string-list)])
        (> (length filtered-list) 0))))

(define number-of-passwords-with-numbers (length (filter contains-number? passwords)))

(define percent-of-passwords-with-numbers (/ number-of-passwords-with-numbers
                                             (length passwords)))

"Percent of passwords that contain at least 1 number:"
percent-of-passwords-with-numbers

#|Average length of password?|#

(define AvLenP (/ (apply + (map string-length passwords)) (length passwords)))

"Average length of the passwords"
AvLenP

#|How many passwords start with capital letter?|#
; (InitCap? str): boolean?
; str: string?
; It returns the boolean of whether the string contains an uppercase letter at its
; first position.
(define InitCap? (lambda (str) 
        (char-upper-case?
            (string-ref str 0)
        )
    )
)

"Number of passwords starting with capital letters"
(define CLSpswrds (length (filter InitCap? passwords)))
CLSpswrds

#|How many lines are in the text?|#

(define number-of-lines (length (string-split text "\n")))

"Number of lines in the text:"
number-of-lines

#|How many words are in the text?|#

(define number-of-words (apply + (map (lambda (x) (length (string-split x "\n"))) (string-split text " "))))

"Number of words in the text:"
number-of-words

#|
Data Visualization

We chose to visualize question #1, "What percent of passwords have at least 1 number?"
We used a bar graph. The bar on the left represents how many passwords have no numbers,
and the bar on the right represents how many passwords have at least 1 number.
|#

(import image)

(define percent-of-passwords-with-no-numbers (- 1 percent-of-passwords-with-numbers))

(define bar-width 100)
(define bar-max-height 500)
(define left-bar-height (* bar-max-height percent-of-passwords-with-no-numbers))
(define right-bar-height (* bar-max-height percent-of-passwords-with-numbers))

(define left-bar (rectangle bar-width left-bar-height "solid" "red"))
(define right-bar (rectangle bar-width right-bar-height "solid" "blue"))
(define bar-padding (rectangle 30 bar-max-height "solid" "white"))

(define bar-comp (beside/align "bottom" left-bar bar-padding right-bar))

(define x-axis (rectangle (* bar-width 3) 2 "solid" "black"))
(define x-slacks (rectangle 1 10 "solid" "black"))
(define lbledX (above/align "right" x-axis x-slacks))
(define y-axis (rectangle 2 (* 1.2 bar-max-height) "solid" "black"))
(define y-slacks (rectangle 10 1 "solid" "black"))
(define lbledY (beside/align "top" y-slacks y-axis))

(define x-comp (above bar-comp lbledX))
(define x&yComp (beside lbledY x-comp))

"Percent of passwords with numbers vs without"
"Red: passwords without numbers"
"Blue: passwords with numbers"

x&yComp