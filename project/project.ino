// Bernardo, Ferrer, Malubag, Villanueva
// ENGG 122.02
// Project

#include <Arduino.h>
#include <Keypad.h>
#include <Wire.h>
#include <Adafruit_GFX.h>
#include <Adafruit_SSD1306.h>

#define SCREEN_WIDTH 128 // OLED display width, in pixels
#define SCREEN_HEIGHT 64 // OLED display height, in pixels

// Declaration for an SSD1306 display connected to I2C (SDA, SCL pins)
Adafruit_SSD1306 display(SCREEN_WIDTH, SCREEN_HEIGHT, &Wire, -1);

const byte ROWS = 4;
const byte COLS = 3;
char hexaKeys[ROWS][COLS] = {
  {'1', '2', '3'},
  {'4', '5', '6'},
  {'7', '8', '9'},
  {'*', '0', '#'}
};

// pins connected to ESP32
byte rowPins[ROWS] = {13, 12, 14, 27};
byte colPins[COLS] = {26, 25, 33};
Keypad customKeypad = Keypad(makeKeymap(hexaKeys), rowPins, colPins, ROWS, COLS);

// timing purposes
unsigned long time_elapsed;

const int GUESS_NUM = 2315;
const int GUESS_LENGTH = 5;
const int LETTERS_NUM = 26;


// wordle dictionary
const String guesses[GUESS_NUM] = {
  "aback","abase","abate","abbey","abbot","abhor","abide","abled","abode","abort","about","above","abuse","abyss","acorn","acrid","actor","acute","adage","adapt","adept","admin","admit","adobe","adopt","adore","adorn","adult","affix","afire","afoot","afoul","after","again","agape","agate","agent","agile","aging","aglow","agony","agora","agree","ahead","aider","aisle","alarm","album","alert","algae","alibi","alien","align","alike","alive","allay","alley","allot","allow","alloy","aloft","alone","along","aloof","aloud","alpha","altar","alter","amass","amaze","amber","amble","amend","amiss","amity","among","ample","amply","amuse","angel","anger","angle","angry","angst","anime","ankle","annex","annoy","annul","anode","antic","anvil","aorta","apart","aphid","aping","apnea","apple","apply","apron","aptly","arbor","ardor","arena","argue","arise","armor","aroma","arose","array","arrow","arson","artsy","ascot","ashen","aside","askew","assay","asset","atoll","atone","attic","audio","audit","augur","aunty","avail","avert","avian","avoid","await","awake","award","aware","awash","awful","awoke","axial","axiom","axion","azure","bacon","badge","badly","bagel","baggy","baker","baler","balmy","banal","banjo","barge","baron","basal","basic","basil","basin","basis","baste","batch","bathe","baton","batty","bawdy","bayou","beach","beady","beard","beast","beech","beefy","befit","began","begat","beget","begin","begun","being","belch","belie","belle","belly","below","bench","beret","berry","berth","beset","betel","bevel","bezel","bible","bicep","biddy","bigot","bilge","billy","binge","bingo","biome","birch","birth","bison","bitty","black","blade","blame","bland","blank","blare","blast","blaze","bleak","bleat","bleed","bleep","blend","bless","blimp","blind","blink","bliss","blitz","bloat","block","bloke","blond","blood","bloom","blown","bluer","bluff","blunt","blurb","blurt","blush","board","boast","bobby","boney","bongo","bonus","booby","boost","booth","booty","booze","boozy","borax","borne","bosom","bossy","botch","bough","boule","bound","bowel","boxer","brace","braid","brain","brake","brand","brash","brass","brave","bravo","brawl","brawn","bread","break","breed","briar","bribe","brick","bride","brief","brine","bring","brink","briny","brisk","broad","broil","broke","brood","brook","broom","broth","brown","brunt","brush","brute","buddy","budge","buggy","bugle","build","built","bulge","bulky","bully","bunch","bunny","burly","burnt","burst","bused","bushy","butch","butte","buxom","buyer","bylaw","cabal","cabby","cabin","cable","cacao","cache","cacti","caddy","cadet","cagey","cairn","camel","cameo","canal","candy","canny","canoe","canon","caper","caput","carat","cargo","carol","carry","carve","caste","catch","cater","catty","caulk","cause","cavil","cease","cedar","cello","chafe","chaff","chain","chair","chalk","champ","chant","chaos","chard","charm","chart","chase","chasm","cheap","cheat","check","cheek","cheer","chess","chest","chick","chide","chief","child","chili","chill","chime","china","chirp","chock","choir","choke","chord","chore","chose","chuck","chump","chunk","churn","chute","cider","cigar","cinch","circa","civic","civil","clack","claim","clamp","clang","clank","clash","clasp","class","clean","clear","cleat","cleft","clerk","click","cliff","climb","cling","clink","cloak","clock","clone","close","cloth","cloud","clout","clove","clown","cluck","clued","clump","clung","coach","coast","cobra","cocoa","colon","color","comet","comfy","comic","comma","conch","condo","conic","copse","coral","corer","corny","couch","cough","could","count","coupe","court","coven","cover","covet","covey","cower","coyly","crack","craft","cramp","crane","crank","crash","crass","crate","crave","crawl","craze","crazy","creak","cream","credo","creed","creek","creep","creme","crepe","crept","cress","crest","crick","cried","crier","crime","crimp","crisp","croak","crock","crone","crony","crook","cross","croup","crowd","crown","crude","cruel","crumb","crump","crush","crust","crypt","cubic","cumin","curio","curly","curry","curse","curve","curvy","cutie","cyber","cycle","cynic","daddy","daily","dairy","daisy","dally","dance","dandy","datum","daunt","dealt","death","debar","debit","debug","debut","decal","decay","decor","decoy","decry","defer","deign","deity","delay","delta","delve","demon","demur","denim","dense","depot","depth","derby","deter","detox","deuce","devil","diary","dicey","digit","dilly","dimly","diner","dingo","dingy","diode","dirge","dirty","disco","ditch","ditto","ditty","diver","dizzy","dodge","dodgy","dogma","doing","dolly","donor","donut","dopey","doubt","dough","dowdy","dowel","downy","dowry","dozen","draft","drain","drake","drama","drank","drape","drawl","drawn","dread","dream","dress","dried","drier","drift","drill","drink","drive","droit","droll","drone","drool","droop","dross","drove","drown","druid","drunk","dryer","dryly","duchy","dully","dummy","dumpy","dunce","dusky","dusty","dutch","duvet","dwarf","dwell","dwelt","dying","eager","eagle","early","earth","easel","eaten","eater","ebony","eclat","edict","edify","eerie","egret","eight","eject","eking","elate","elbow","elder","elect","elegy","elfin","elide","elite","elope","elude","email","embed","ember","emcee","empty","enact","endow","enema","enemy","enjoy","ennui","ensue","enter","entry","envoy","epoch","epoxy","equal","equip","erase","erect","erode","error","erupt","essay","ester","ether","ethic","ethos","etude","evade","event","every","evict","evoke","exact","exalt","excel","exert","exile","exist","expel","extol","extra","exult","eying","fable","facet","faint","fairy","faith","false","fancy","fanny","farce","fatal","fatty","fault","fauna","favor","feast","fecal","feign","fella","felon","femme","femur","fence","feral","ferry","fetal","fetch","fetid","fetus","fever","fewer","fiber","fibre","ficus","field","fiend","fiery","fifth","fifty","fight","filer","filet","filly","filmy","filth","final","finch","finer","first","fishy","fixer","fizzy","fjord","flack","flail","flair","flake","flaky","flame","flank","flare","flash","flask","fleck","fleet","flesh","flick","flier","fling","flint","flirt","float","flock","flood","floor","flora","floss","flour","flout","flown","fluff","fluid","fluke","flume","flung","flunk","flush","flute","flyer","foamy","focal","focus","foggy","foist","folio","folly","foray","force","forge","forgo","forte","forth","forty","forum","found","foyer","frail","frame","frank","fraud","freak","freed","freer","fresh","friar","fried","frill","frisk","fritz","frock","frond","front","frost","froth","frown","froze","fruit","fudge","fugue","fully","fungi","funky","funny","furor","furry","fussy","fuzzy","gaffe","gaily","gamer","gamma","gamut","gassy","gaudy","gauge","gaunt","gauze","gavel","gawky","gayer","gayly","gazer","gecko","geeky","geese","genie","genre","ghost","ghoul","giant","giddy","gipsy","girly","girth","given","giver","glade","gland","glare","glass","glaze","gleam","glean","glide","glint","gloat","globe","gloom","glory","gloss","glove","glyph","gnash","gnome","godly","going","golem","golly","gonad","goner","goody","gooey","goofy","goose","gorge","gouge","gourd","grace","grade","graft","grail","grain","grand","grant","grape","graph","grasp","grass","grate","grave","gravy","graze","great","greed","green","greet","grief","grill","grime","grimy","grind","gripe","groan","groin","groom","grope","gross","group","grout","grove","growl","grown","gruel","gruff","grunt","guard","guava","guess","guest","guide","guild","guile","guilt","guise","gulch","gully","gumbo","gummy","guppy","gusto","gusty","gypsy","habit","hairy","halve","handy","happy","hardy","harem","harpy","harry","harsh","haste","hasty","hatch","hater","haunt","haute","haven","havoc","hazel","heady","heard","heart","heath","heave","heavy","hedge","hefty","heist","helix","hello","hence","heron","hilly","hinge","hippo","hippy","hitch","hoard","hobby","hoist","holly","homer","honey","honor","horde","horny","horse","hotel","hotly","hound","house","hovel","hover","howdy","human","humid","humor","humph","humus","hunch","hunky","hurry","husky","hussy","hutch","hydro","hyena","hymen","hyper","icily","icing","ideal","idiom","idiot","idler","idyll","igloo","iliac","image","imbue","impel","imply","inane","inbox","incur","index","inept","inert","infer","ingot","inlay","inlet","inner","input","inter","intro","ionic","irate","irony","islet","issue","itchy","ivory","jaunt","jazzy","jelly","jerky","jetty","jewel","jiffy","joint","joist","joker","jolly","joust","judge","juice","juicy","jumbo","jumpy","junta","junto","juror","kappa","karma","kayak","kebab","khaki","kinky","kiosk","kitty","knack","knave","knead","kneed","kneel","knelt","knife","knock","knoll","known","koala","krill","label","labor","laden","ladle","lager","lance","lanky","lapel","lapse","large","larva","lasso","latch","later","lathe","latte","laugh","layer","leach","leafy","leaky","leant","leapt","learn","lease","leash","least","leave","ledge","leech","leery","lefty","legal","leggy","lemon","lemur","leper","level","lever","libel","liege","light","liken","lilac","limbo","limit","linen","liner","lingo","lipid","lithe","liver","livid","llama","loamy","loath","lobby","local","locus","lodge","lofty","logic","login","loopy","loose","lorry","loser","louse","lousy","lover","lower","lowly","loyal","lucid","lucky","lumen","lumpy","lunar","lunch","lunge","lupus","lurch","lurid","lusty","lying","lymph","lynch","lyric","macaw","macho","macro","madam","madly","mafia","magic","magma","maize","major","maker","mambo","mamma","mammy","manga","mange","mango","mangy","mania","manic","manly","manor","maple","march","marry","marsh","mason","masse","match","matey","mauve","maxim","maybe","mayor","mealy","meant","meaty","mecca","medal","media","medic","melee","melon","mercy","merge","merit","merry","metal","meter","metro","micro","midge","midst","might","milky","mimic","mince","miner","minim","minor","minty","minus","mirth","miser","missy","mocha","modal","model","modem","mogul","moist","molar","moldy","money","month","moody","moose","moral","moron","morph","mossy","motel","motif","motor","motto","moult","mound","mount","mourn","mouse","mouth","mover","movie","mower","mucky","mucus","muddy","mulch","mummy","munch","mural","murky","mushy","music","musky","musty","myrrh","nadir","naive","nanny","nasal","nasty","natal","naval","navel","needy","neigh","nerdy","nerve","never","newer","newly","nicer","niche","niece","night","ninja","ninny","ninth","noble","nobly","noise","noisy","nomad","noose","north","nosey","notch","novel","nudge","nurse","nutty","nylon","nymph","oaken","obese","occur","ocean","octal","octet","odder","oddly","offal","offer","often","olden","older","olive","ombre","omega","onion","onset","opera","opine","opium","optic","orbit","order","organ","other","otter","ought","ounce","outdo","outer","outgo","ovary","ovate","overt","ovine","ovoid","owing","owner","oxide","ozone","paddy","pagan","paint","paler","palsy","panel","panic","pansy","papal","paper","parer","parka","parry","parse","party","pasta","paste","pasty","patch","patio","patsy","patty","pause","payee","payer","peace","peach","pearl","pecan","pedal","penal","pence","penne","penny","perch","peril","perky","pesky","pesto","petal","petty","phase","phone","phony","photo","piano","picky","piece","piety","piggy","pilot","pinch","piney","pinky","pinto","piper","pique","pitch","pithy","pivot","pixel","pixie","pizza","place","plaid","plain","plait","plane","plank","plant","plate","plaza","plead","pleat","plied","plier","pluck","plumb","plume","plump","plunk","plush","poesy","point","poise","poker","polar","polka","polyp","pooch","poppy","porch","poser","posit","posse","pouch","pound","pouty","power","prank","prawn","preen","press","price","prick","pride","pried","prime","primo","print","prior","prism","privy","prize","probe","prone","prong","proof","prose","proud","prove","prowl","proxy","prude","prune","psalm","pubic","pudgy","puffy","pulpy","pulse","punch","pupal","pupil","puppy","puree","purer","purge","purse","pushy","putty","pygmy","quack","quail","quake","qualm","quark","quart","quash","quasi","queen","queer","quell","query","quest","queue","quick","quiet","quill","quilt","quirk","quite","quota","quote","quoth","rabbi","rabid","racer","radar","radii","radio","rainy","raise","rajah","rally","ralph","ramen","ranch","randy","range","rapid","rarer","raspy","ratio","ratty","raven","rayon","razor","reach","react","ready","realm","rearm","rebar","rebel","rebus","rebut","recap","recur","recut","reedy","refer","refit","regal","rehab","reign","relax","relay","relic","remit","renal","renew","repay","repel","reply","rerun","reset","resin","retch","retro","retry","reuse","revel","revue","rhino","rhyme","rider","ridge","rifle","right","rigid","rigor","rinse","ripen","riper","risen","riser","risky","rival","river","rivet","roach","roast","robin","robot","rocky","rodeo","roger","rogue","roomy","roost","rotor","rouge","rough","round","rouse","route","rover","rowdy","rower","royal","ruddy","ruder","rugby","ruler","rumba","rumor","rupee","rural","rusty","sadly","safer","saint","salad","sally","salon","salsa","salty","salve","salvo","sandy","saner","sappy","sassy","satin","satyr","sauce","saucy","sauna","saute","savor","savoy","savvy","scald","scale","scalp","scaly","scamp","scant","scare","scarf","scary","scene","scent","scion","scoff","scold","scone","scoop","scope","score","scorn","scour","scout","scowl","scram","scrap","scree","screw","scrub","scrum","scuba","sedan","seedy","segue","seize","semen","sense","sepia","serif","serum","serve","setup","seven","sever","sewer","shack","shade","shady","shaft","shake","shaky","shale","shall","shalt","shame","shank","shape","shard","share","shark","sharp","shave","shawl","shear","sheen","sheep","sheer","sheet","sheik","shelf","shell","shied","shift","shine","shiny","shire","shirk","shirt","shoal","shock","shone","shook","shoot","shore","shorn","short","shout","shove","shown","showy","shrew","shrub","shrug","shuck","shunt","shush","shyly","siege","sieve","sight","sigma","silky","silly","since","sinew","singe","siren","sissy","sixth","sixty","skate","skier","skiff","skill","skimp","skirt","skulk","skull","skunk","slack","slain","slang","slant","slash","slate","slave","sleek","sleep","sleet","slept","slice","slick","slide","slime","slimy","sling","slink","sloop","slope","slosh","sloth","slump","slung","slunk","slurp","slush","slyly","smack","small","smart","smash","smear","smell","smelt","smile","smirk","smite","smith","smock","smoke","smoky","smote","snack","snail","snake","snaky","snare","snarl","sneak","sneer","snide","sniff","snipe","snoop","snore","snort","snout","snowy","snuck","snuff","soapy","sober","soggy","solar","solid","solve","sonar","sonic","sooth","sooty","sorry","sound","south","sower","space","spade","spank","spare","spark","spasm","spawn","speak","spear","speck","speed","spell","spelt","spend","spent","sperm","spice","spicy","spied","spiel","spike","spiky","spill","spilt","spine","spiny","spire","spite","splat","split","spoil","spoke","spoof","spook","spool","spoon","spore","sport","spout","spray","spree","sprig","spunk","spurn","spurt","squad","squat","squib","stack","staff","stage","staid","stain","stair","stake","stale","stalk","stall","stamp","stand","stank","stare","stark","start","stash","state","stave","stead","steak","steal","steam","steed","steel","steep","steer","stein","stern","stick","stiff","still","stilt","sting","stink","stint","stock","stoic","stoke","stole","stomp","stone","stony","stood","stool","stoop","store","stork","storm","story","stout","stove","strap","straw","stray","strip","strut","stuck","study","stuff","stump","stung","stunk","stunt","style","suave","sugar","suing","suite","sulky","sully","sumac","sunny","super","surer","surge","surly","sushi","swami","swamp","swarm","swash","swath","swear","sweat","sweep","sweet","swell","swept","swift","swill","swine","swing","swirl","swish","swoon","swoop","sword","swore","sworn","swung","synod","syrup","tabby","table","taboo","tacit","tacky","taffy","taint","taken","taker","tally","talon","tamer","tango","tangy","taper","tapir","tardy","tarot","taste","tasty","tatty","taunt","tawny","teach","teary","tease","teddy","teeth","tempo","tenet","tenor","tense","tenth","tepee","tepid","terra","terse","testy","thank","theft","their","theme","there","these","theta","thick","thief","thigh","thing","think","third","thong","thorn","those","three","threw","throb","throw","thrum","thumb","thump","thyme","tiara","tibia","tidal","tiger","tight","tilde","timer","timid","tipsy","titan","tithe","title","toast","today","toddy","token","tonal","tonga","tonic","tooth","topaz","topic","torch","torso","torus","total","totem","touch","tough","towel","tower","toxic","toxin","trace","track","tract","trade","trail","train","trait","tramp","trash","trawl","tread","treat","trend","triad","trial","tribe","trice","trick","tried","tripe","trite","troll","troop","trope","trout","trove","truce","truck","truer","truly","trump","trunk","truss","trust","truth","tryst","tubal","tuber","tulip","tulle","tumor","tunic","turbo","tutor","twang","tweak","tweed","tweet","twice","twine","twirl","twist","twixt","tying","udder","ulcer","ultra","umbra","uncle","uncut","under","undid","undue","unfed","unfit","unify","union","unite","unity","unlit","unmet","unset","untie","until","unwed","unzip","upper","upset","urban","urine","usage","usher","using","usual","usurp","utile","utter","vague","valet","valid","valor","value","valve","vapid","vapor","vault","vaunt","vegan","venom","venue","verge","verse","verso","verve","vicar","video","vigil","vigor","villa","vinyl","viola","viper","viral","virus","visit","visor","vista","vital","vivid","vixen","vocal","vodka","vogue","voice","voila","vomit","voter","vouch","vowel","vying","wacky","wafer","wager","wagon","waist","waive","waltz","warty","waste","watch","water","waver","waxen","weary","weave","wedge","weedy","weigh","weird","welch","welsh","wench","whack","whale","wharf","wheat","wheel","whelp","where","which","whiff","while","whine","whiny","whirl","whisk","white","whole","whoop","whose","widen","wider","widow","width","wield","wight","willy","wimpy","wince","winch","windy","wiser","wispy","witch","witty","woken","woman","women","woody","wooer","wooly","woozy","wordy","world","worry","worse","worst","worth","would","wound","woven","wrack","wrath","wreak","wreck","wrest","wring","wrist","write","wrong","wrote","wrung","wryly","yacht","yearn","yeast","yield","young","youth","zebra","zesty","zonal"
};


// returns a random solution word from the dictionary of guesses
String chooseSolutionWord() {
  return guesses[random(0, GUESS_NUM)];
}


// colors: 0 - GRAY, 1 - YELLOW, 2 - GREEN
void getColors(String guess, String solution, int (&colors)[5]) {
  // assign GREEN color to correct positions
  for (int i = 0; i < GUESS_LENGTH; i++) {
    if (guess[i] == solution[i]) colors[i] = 2;
    else colors[i] = 0;
  }

  // assign YELLOW color to characters in wrong positions
  for (int i = 0; i < GUESS_LENGTH; i++) {
    if (colors[i] == 2) continue;  // only compare when GRAY

    for (int j = 0; j < GUESS_LENGTH; j++) {
      if (i == j) continue;  // only YELLOW when positions are different
      if (guess[i] != solution[j]) continue;  // if not same letter, skip check

      // count number of letters to check how many YELLOWS to display
      int same = 0;
      for (int k = 0; k <= GUESS_LENGTH; k++) {
        if (solution[j] == solution[k]) same++;  // count same letters
        if (solution[j] == guess[k] && colors[k] != 0) same--;  // count already YELLOW or GREEN in guess
      }

      if (same > 0) colors[i] = 1;
    }
  }
}


void setup() {
  Serial.begin(115200); // initialize serial communication
  randomSeed(analogRead(2)); // randomizes seed for random word
 
  display.setRotation(2); // rotates LCD
  if(!display.begin(SSD1306_SWITCHCAPVCC, 0x3C)) { // Address 0x3D for 128x64
    Serial.println(F("SSD1306 allocation failed"));
    for(;;);
  }
  delay(2000);
  display.clearDisplay();
}

void loop() {
  title_screen();
  while (true)
  {
    char get_enter;
    do
    {
      get_enter = customKeypad.getKey();
    } while (get_enter != '0');

    // get random word from dictionary
    String solution = chooseSolutionWord();
    int attempt = 0;
    bool correct;

    // testing purposes
    Serial.print("Word: ");
    Serial.println(solution);

    // display game screen
    game_screen();
   
    // input guess loop
    while (attempt < 6)
    {
      int colors[5] = {0, 0, 0, 0, 0};
      char guess[5] = {0, 0, 0, 0, 0};
      String guess_final;
      bool enter_key = false;

      // cycling through the letters
      int letter_pointer = 0;
      int press_count = 0;
      char previous_key;

      // key input loop
      while (true)
      {
        // replace display
        guess_input(guess, letter_pointer);

        char customKey = customKeypad.getKey();
        char letter;
        bool arrow;
        if (arrow) empty_guess (40, 15, 48, 15);
        arrow = false;

        // blinking cursor
        if ((millis() / 500) % 2 == 0)
          display.drawLine (40 + (letter_pointer * 10), 15, 48 + (letter_pointer * 10), 15, WHITE);
        else
          display.drawLine (40 + (letter_pointer * 10), 15, 48 + (letter_pointer * 10), 15, BLACK);

        // if button pressed
        if (customKey) {
          // cycle through letters
          if ((millis() - time_elapsed <= 500) && (previous_key == customKey)) { // 500ms waiting time
            press_count++; // increment counter
            time_elapsed = millis();
          }
          else {
            previous_key = customKey;
            press_count = 0;
            time_elapsed = millis();
          }

          // from key pressed to corresponding letter
          switch (previous_key) {
            case '2':
              switch (press_count) {
                case 0: letter = 'A'; break;
                case 1: letter = 'B'; break;
                case 2: letter = 'C'; break;
              }
              break;
            case '3':
              switch (press_count) {
                case 0: letter = 'D'; break;
                case 1: letter = 'E'; break;
                case 2: letter = 'F'; break;
              }
              break;
            case '4':
              switch (press_count) {
                case 0: letter = 'G'; break;
                case 1: letter = 'H'; break;
                case 2: letter = 'I'; break;
              }
              break;
            case '5':
              switch (press_count) {
                case 0: letter = 'J'; break;
                case 1: letter = 'K'; break;
                case 2: letter = 'L'; break;
              }
              break;
            case '6':
              switch (press_count) {
                case 0: letter = 'M'; break;
                case 1: letter = 'N'; break;
                case 2: letter = 'O'; break;
              }
              break;
            case '7':
              switch (press_count) {
                case 0: letter = 'P'; break;
                case 1: letter = 'Q'; break;
                case 2: letter = 'R'; break;
                case 3: letter = 'S'; break;
              }
              break;
            case '8':
              switch (press_count) {
                case 0: letter = 'T'; break;
                case 1: letter = 'U'; break;
                case 2: letter = 'V'; break;
              }
              break;
            case '9':
              switch (press_count) {
                case 0: letter = 'W'; break;
                case 1: letter = 'X'; break;
                case 2: letter = 'Y'; break;
                case 3: letter = 'Z'; break;
              }
              break;
            case '*':
              if (letter_pointer > 0) letter_pointer -= 1;
              arrow = true;
              break;
            case '#':
              if (letter_pointer < GUESS_LENGTH - 1) letter_pointer += 1;
              arrow = true;
              break;
            case '0': enter_key = true; break;
          }

          if (arrow) continue; // arrow button was pressed

          for (int i = 0; i < GUESS_LENGTH; i++)
          {
            if (guess[i] == 0) enter_key = false; // if not all letters are filled
          }

          if (enter_key) break; // break out of input loop
          guess[letter_pointer] = letter;

          // for serial communication
          Serial.print("Guess: ");
          Serial.print(guess[0]);
          Serial.print(guess[1]);
          Serial.print(guess[2]);
          Serial.print(guess[3]);
          Serial.println(guess[4]);


        }
      }
      attempt++;

      // array of characters to string
      for (char c: guess)
      {
          guess_final += c;
      }

      // annotate each letter with "colors"
      solution.toUpperCase();
      getColors(guess_final, solution, colors);

      switch (attempt) {
        case 1: print_guess (10, 22, 12, 24, guess, colors); break;
        case 2: print_guess (10, 36, 12, 38, guess, colors); break;
        case 3: print_guess (10, 50, 12, 52, guess, colors); break;
        case 4: print_guess (70, 22, 72, 24, guess, colors); break;
        case 5: print_guess (70, 36, 72, 38, guess, colors); break;
        case 6: print_guess (70, 50, 72, 52, guess, colors); break;
      }

      // check if correct answer
      correct = true;
      for (int i: colors) {
        if (i == 0) correct = false;
      }
      if (correct) break;

    }

    if (correct) {
      correct_screen(attempt);
    } else {
      incorrect_screen(solution);
    }
  }
}


void game_screen () {
  display.clearDisplay();
  empty_guess (40, 15, 48, 15); // input line
  empty_guess (10, 31, 18, 31); // guess 1
  empty_guess (10, 45, 18, 45); // guess 2
  empty_guess (10, 59, 18, 59); // guess 3
  empty_guess (70, 31, 78, 31); // guess 4
  empty_guess (70, 45, 78, 45); // guess 5
  empty_guess (70, 59, 78, 59); // guess 6

  display.display();
}


// displays lines
void empty_guess (int lineX1, int lineY1, int lineX2, int lineY2) {
  for (int i=0; i<=4; i++)
    display.drawLine(lineX1+(i*10), lineY1, lineX2+(i*10), lineY2, WHITE);
}


// displays letters
void guess_input (char* guess, int i) {
  display.fillRect(40 + (i * 10), 6, 9, 9, BLACK);
  display.setTextSize(1);
  display.setTextColor(WHITE);
  display.setCursor(42 + (i * 10), 8);
  display.println(guess[i]);
  display.display();
}


// print letter with indicator
void print_letter (int charX, int charY, char* guess, int* color, int i) {
  display.setTextSize(1);
  if (color[i] == 2) display.setTextColor(BLACK);
  else display.setTextColor(WHITE);
  display.setCursor(charX+(i*10), charY);
  display.println(guess[i]);
}


// print guess attempt
void print_guess (int boxX, int boxY, int charX, int charY, char* guess, int* color) {
  // empties the input line
  for (int i = 0; i < 5; i++) display.fillRect(40 + (i * 10), 6, 9, 9, BLACK);
  empty_guess (40, 15, 48, 15);
  for (int i=0; i<=4; i++) {
    print_letter (charX, charY, guess, color, i);
    if (color[i] == 1) { // yellow
      display.drawRect(boxX+(i*10), boxY, 9, 11, WHITE);
      print_letter (charX, charY, guess, color, i);
    } 
    else if (color[i] == 2) { // green
      display.fillRect(boxX+(i*10), boxY, 9, 11, WHITE);
      print_letter (charX, charY, guess, color, i);
    }
  }
}

// title screen
void title_screen() {
  display.setTextSize(2);
  display.setTextColor(WHITE);
  display.setCursor(27, 15);
  display.println("WORDLE");

  display.setTextSize(1);
  display.setTextColor(WHITE);
  display.setCursor(35, 38);
  display.println("PRESS [0]");

  display.setTextSize(1);
  display.setTextColor(WHITE);
  display.setCursor(7, 46);
  display.println("TO START A NEW GAME");

  display.display();
}


// game over screen
void incorrect_screen(String solution) {
  display.clearDisplay();

  display.setTextSize(1);
  display.setTextColor(WHITE);
  display.setCursor(8, 10);
  display.print("THE WORD WAS: ");
  display.println(solution);

  display.setTextSize(2);
  display.setTextColor(WHITE);
  display.setCursor(12, 21);
  display.println("INCORRECT");

  display.setTextSize(1);
  display.setTextColor(WHITE);
  display.setCursor(21, 40);
  display.println("START NEW GAME?");

  display.setTextSize(1);
  display.setTextColor(WHITE);
  display.setCursor(35, 49);
  display.println("PRESS [0]");

  display.display();
}


// you win screen
void correct_screen(int attempt) {
  display.clearDisplay();

  if (attempt == 1) {
    display.setTextSize(1);
    display.setTextColor(WHITE);
    display.setCursor(39, 10);
    display.print("IN ");
    display.print(attempt);
    display.println(" TRY,");
  } 
  else {
    display.setTextSize(1);
    display.setTextColor(WHITE);
    display.setCursor(34, 10);
    display.print("IN ");
    display.print(attempt);
    display.println(" TRIES,");
  }

  display.setTextSize(2);
  display.setTextColor(WHITE);
  display.setCursor(18, 21);
  display.println("CORRECT!");

  display.setTextSize(1);
  display.setTextColor(WHITE);
  display.setCursor(21, 40);
  display.println("START NEW GAME?");

  display.setTextSize(1);
  display.setTextColor(WHITE);
  display.setCursor(35, 49);
  display.println("PRESS [0]");

  display.display();
}
