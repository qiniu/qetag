package com.qiniu;

/*
java 最新版本 
https://github.com/qiniu/java-sdk/blob/master/src/main/java/com/qiniu/util/Etag.java
android 最新版本
https://github.com/qiniu/android-sdk/blob/master/library/src/main/java/com/qiniu/android/utils/Etag.java
*/

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import javax.xml.bind.DatatypeConverter;

public class QETag {
	private final int CHUNK_SIZE = 1 << 22;
	private final int BUFFER_SIZE = 1 << 16;

	public String urlSafeBase64Encode(byte[] data) {
		String encodedString = DatatypeConverter.printBase64Binary(data);
		encodedString = encodedString.replace('+', '-').replace('/', '_');
		return encodedString;
	}

	public String calcETag(String fileName) throws IOException,
			NoSuchAlgorithmException {
		String etag = "";
		File file = new File(fileName);
		if (!(file.exists() && file.isFile() && file.canRead())) {
			System.err.println("Error: File not found or not readable");
			return etag;
		}
		FileInputStream inputStream = new FileInputStream(file);
		MessageDigest chunkDataDigest = MessageDigest.getInstance("sha1");
		MessageDigest allDigestDigest = MessageDigest.getInstance("sha1");
		byte[] buffer = new byte[BUFFER_SIZE];
		int multi = CHUNK_SIZE / BUFFER_SIZE;
		int count = 0;
		int bytesRead;
		boolean bigFlag = false;
		while ((bytesRead = inputStream.read(buffer)) > 0) {
			if (++count > multi) {
				bigFlag = true;
				allDigestDigest.update(chunkDataDigest.digest());
				chunkDataDigest.reset();
				count = 0;
			}
			chunkDataDigest.update(buffer, 0, bytesRead);
		}
		byte[] hashData = new byte[21];
		if (bigFlag) {
			hashData[0] = (byte) 0x96;
			System.arraycopy(allDigestDigest.digest(chunkDataDigest.digest()), 0, hashData, 1, 20);
		} else {
			hashData[0] = 0x16;
			System.arraycopy(chunkDataDigest.digest(), 0, hashData, 1, 20);
		}
		etag = urlSafeBase64Encode(hashData);
		inputStream.close();
		return etag;
	}

	public static void main(String[] args) {
		if (args.length != 1) {
			System.out.println("Usage: qetag <filename>");
		} else {
			String fileName = args[0];
			QETag etag = new QETag();
			try {
				System.out.println(etag.calcETag(fileName));
			} catch (NoSuchAlgorithmException ex) {
				System.err.println("Unsupported algorithm:" + ex.getMessage());
			} catch (IOException ex) {
				System.err.println("IO Error:" + ex.getMessage());
			}
		}
	}
}
