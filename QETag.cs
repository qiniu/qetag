using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using System.Threading.Tasks;

namespace ConsoleApplication2
{
    class QETag
    {
    private int CHUNK_SIZE = 1 << 22;

	public byte[] sha1(byte[] data)  {
        return System.Security.Cryptography.SHA1.Create().ComputeHash(data);
	}

	public String urlSafeBase64Encode(byte[] data) {
        String encodedString = Convert.ToBase64String(data);
        encodedString = encodedString.Replace('+', '-').Replace('/', '_');
		return encodedString;
	}
        static void Main(string[] args)
        {
            Console.WriteLine("Usage: qetag <path>-----" + @"c:\test\MyTest.txt");
            string path = Console.ReadLine();
			QETag etag = new QETag();
			try {
                Console.WriteLine(etag.calcETag(path));
			} catch (Exception ex) {
                Console.WriteLine("Unsupported algorithm:" + ex.Message);
			} 
		
            Console.WriteLine("algorithm-hash-etag");
            Console.Read();
        }
        public String calcETag(String path)
        {
            if (File.Exists(path))
            {
                File.Delete(path);
            }
            String etag = "";
            FileStream fs ;
            using ( fs= File.Create(path))
            {
                Byte[] info = new UTF8Encoding(true).GetBytes("This is some text in the file.");
                fs.Write(info, 0, info.Length);
            }
             fs = File.OpenRead(path);
            long fileLength = fs.Length;
            if (fileLength <= CHUNK_SIZE)
            {
                byte[] fileData = new byte[(int)fileLength];
                fs.Read(fileData, 0, (int)fileLength);
                byte[] sha1Data = sha1(fileData);
                int sha1DataLen = sha1Data.Length;
                byte[] hashData = new byte[sha1DataLen + 1];
               
                System.Array.Copy(sha1Data, 0, hashData, 1, sha1DataLen);
                hashData[0] = 0x16;
                etag = urlSafeBase64Encode(hashData);
            }
            else
            {
                int chunkCount = (int)(fileLength / CHUNK_SIZE);
                if (fileLength % CHUNK_SIZE != 0)
                {
                    chunkCount += 1;
                }
                byte[] allSha1Data = new byte[0];
                for (int i = 0; i < chunkCount; i++)
                {
                    byte[] chunkData = new byte[CHUNK_SIZE];
                    int bytesReadLen = fs.Read(chunkData, 0, CHUNK_SIZE);
                    byte[] bytesRead = new byte[bytesReadLen];
                    System.Array.Copy(chunkData, 0, bytesRead, 0, bytesReadLen);
                    byte[] chunkDataSha1 = sha1(bytesRead);
                    byte[] newAllSha1Data = new byte[chunkDataSha1.Length 
                            + allSha1Data.Length ];
                    System.Array.Copy(allSha1Data, 0, newAllSha1Data, 0,
                            allSha1Data.Length );
                    System.Array.Copy(chunkDataSha1, 0, newAllSha1Data,
                            allSha1Data.Length , chunkDataSha1.Length );
                    allSha1Data = newAllSha1Data;
                }
                byte[] allSha1DataSha1 = sha1(allSha1Data);
                byte[] hashData = new byte[allSha1DataSha1.Length  + 1];
                System.Array.Copy(allSha1DataSha1, 0, hashData, 1,
                        allSha1DataSha1.Length );
                hashData[0] = (byte)0x96;
                etag = urlSafeBase64Encode(hashData);
            }
            fs.Close();
            return etag;

        }
    }
}
