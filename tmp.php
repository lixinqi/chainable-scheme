<?php
//$string = " (I 源代码   (will be) 'Back') ";
//$string = "'Back\\\\' '\'' \"nice\" \"\\\\\\\"\"";
//$string = '"\\\\\\""';
$string = '(("3" ban) xuexichengji-gaikuang)';

define('LIX_TOKEN_SEP', 'SEP');
define('LIX_TOKEN_IDENTIFIER', 'IDENTIFIER');
define('LIX_TOKEN_LITERAL', 'LITERAL');
define('LIX_TOKEN_EXPR', 'EXPR');
$lex = function ($string) {
  $tokens = [];
  while ($string) {
    switch ($string[0]) {
    case " ":
    case "\t":
    case "\n":
    case "\r":
      list($s) = sscanf($string, "%[ \t\n\r]");
      $string = substr($string, strlen($s));
      $tokens[] = [LIX_TOKEN_SEP, $s];
      break;
    case "(":
    case ")":
    case "[":
    case "]":
    case "{":
    case "}":
      $s = $string[0];
      $string = substr($string, 1);
      $tokens[] = [$s, $s];
      break;
    case '"':
      $str = "";
      $string = substr($string, 1);
      if (!$string) {
        die('unclosed "' . "\n");
      }
      while (1) {
        list($s) = sscanf($string, '%[^"]');
        $sLength = strlen($s);
        $string = substr($string, $sLength);
        $backslackCount = 0;
        for ($i = $sLength - 1; $i >= 0; $i --) {
          if ($s[$i] != '\\') {
            break;
          }
          $backslackCount ++;
        }
        $str .= $s;
        $string = substr($string, 1);
        if ($backslackCount % 2 == 0) {
          break;
        } else {
          if (!$string) {
            die('unclosed "' . "\n");
          }
          $str .= '"';
        }
      }
      $str = str_replace([
        '\\\\',
        '\t',
        '\r',
        '\n',
        '\\"',
        ], [
        "\\",
        "\t",
        "\r",
        "\n",
        '"',
        ], $str);
      $tokens[] = [LIX_TOKEN_LITERAL, $str];
      break;
    case "'":
      $str = "";
      $string = substr($string, 1);
      if (!$string) {
        die('unclosed \'' . "\n");
      }
      while (1) {
        list($s) = sscanf($string, "%[^']");
        $sLength = strlen($s);
        $string = substr($string, $sLength);
        $backslackCount = 0;
        for ($i = $sLength - 1; $i >= 0; $i --) {
          if ($s[$i] != '\\') {
            break;
          }
          $backslackCount ++;
        }
        $str .= $s;
        $string = substr($string, 1);
        if ($backslackCount % 2 == 0) {
          break;
        } else {
          if (!$string) {
            die('unclosed \'' . "\n");
          }
          $str .= "'";
        }
      }
      $str = str_replace([
        '\\\\',
        '\t',
        '\r',
        '\n',
        "\\'",
        ], [
        "\\",
        "\t",
        "\r",
        "\n",
        "'",
        ], $str);
      $tokens[] = [LIX_TOKEN_LITERAL, $str];
      break;
    default:
      list($s) = sscanf($string, "%[^ \t\n\r()[]{}'\"]");
      $string = substr($string, strlen($s));
      $tokens[] = [LIX_TOKEN_IDENTIFIER, $s];
    }
  }
  return $tokens;
};

$parse = function ($tokens) {
  $stack = [];
  $stack[] = ['(', '('];
  $stack[] = [
    [LIX_TOKEN_IDENTIFIER, 'begin']
  ];
  foreach ($tokens as $token) {
    switch ($token[0]) {
    case '(':
      $stack[] = $token;
      $stack[] = [];
      break;
    case ')':
      $expr = $stack[count($stack) - 1];
      array_pop($stack);
      array_pop($stack);
      if ($stack[count($stack) - 2][0] != '(') {
        die("unclosed (\n");
      }
      $stack[count($stack) - 1][] = $expr;
      break;
    case '[':
      die('unsupported [');
      break;
    case ']':
      die('unsupported ]');
      break;
    case '{':
      die('unsupported {');
      break;
    case '}':
      die('unsupported }');
      break;
    case LIX_TOKEN_LITERAL:
    case LIX_TOKEN_IDENTIFIER:
      $stack[count($stack) - 1][] = $token;
      break;
    }
  }
  return $stack[count($stack) - 1];
};

$tokens = $lex($string);
$tree = $parse($tokens);

var_dump($tree);

class LixASTNode {

  public function make(array $node) {
  }

  public function generateCode(array $node) {
  }
}

class BeginNode extends LixASTNode {
}

class ExprNode extends LixASTNode {
}

