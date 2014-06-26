# phonetical

If you have ever tried to generate text to speech and been thwarted by the weird vocabulary of your target text.

Look no further!  you can custom design simple dictionaries that will allow replacement and ease the process for your text to speech engine.

Again, this is not a text to speech engine, it a text to speech helper. So that your preferred tts engine says things right.

## Usage


$> let dict = mkDictionary "[{'find':'.', 'use':'dot' }  ,{'find':{'start':'O(','end':')'},'use':'Big O of '}]"

$> phonetic dict "x.y results in O(n)"
  "x dot y results in Big O of n"
