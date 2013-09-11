<?php 

define('BLOCK_BITS', 22);
define('BLOCK_SIZE', 1 << BLOCK_BITS);


error_reporting(E_ERROR | E_WARNING | E_PARSE);


function pack_array($v, $a) {
    return call_user_func_array(pack, array_merge(array($v),(array)$a));
}


function BlockCount($fsize) {
    return (($fsize + (BLOCK_SIZE - 1)) >> BLOCK_BITS);
}

function Qiniu_Encode($str) // URLSafeBase64Encode
{
    $find = array('+', '/');
    $replace = array('-', '_');
    return str_replace($find, $replace, base64_encode($str));
}

function BlockSlicer($fdata) {
    $fsize = strlen($fdata);
    $blocks = str_split($fdata, BLOCK_SIZE);
    $blockCnt = count($blocks);
    return array($fsize, $blocks, $blockCnt);
}


function CalSha1($fdata) {
    $sha1Str = sha1($fdata, true);
    $err = error_get_last();
    if ($err != null) {
        return array(null, $err);
    }
    $byteArray = unpack('C*', $sha1Str);
    return array($byteArray, null);
}


function GetEtag($filename) {
    $fdata = file_get_contents($filename);
    $err = error_get_last();
    if ($err != null) {
        return array(null, $err);
    }

    list($fsize, $blocks, $blockCnt) = BlockSlicer($fdata);
    $sha1Buf = array();

    if ($blockCnt <= 1) {
        $sha1Buf[] = 0x16;
        list($sha1Code, $err) = CalSha1($fdata);
        if ($err != null) {
            return array(null, $err);
        }
        $sha1Buf = array_merge($sha1Buf, $sha1Code);
    } else {
        $sha1Buf[] = 0x96;
        $sha1BlockBuf = array();
        for ($i=0; $i < $blockCnt; $i++) {
            list($sha1Code, $err) = CalSha1($blocks[$i]);
            if ($err != null) {
                return array(null, $err);
            }
            $sha1BlockBuf = array_merge($sha1BlockBuf, $shaCode);
        }
        $tmpData = pack_array('C*', $sha1BlockBuf);
        list($sha1Final, $_err) = CalSha1($tmpData);
        $sha1Buf = array_merge($sha1Buf, $sha1Final);
    }
    $etag = Qiniu_Encode(pack_array('C*', $sha1Buf));
    return array($etag, null);
}




$localfile = "/home/dtynn/Pictures/logo.png";
$localfile1 = "/home/dtynn/Pictures/logo.png1";

list($ret, $err) = GetEtag($localfile);
var_dump($ret);
var_dump($err);
#list($ret1, $err1) = GetEtag($localfile1);
#var_dump($ret1);
#var_dump($err1);
#list($ret2, $err2) = CalSha1(array());
#var_dump($ret2);
#var_dump($err2);
?>

