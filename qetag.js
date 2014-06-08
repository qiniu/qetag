function getEtag(buffer){
	// sha1算法
	var sha1 = function(content){
		var crypto = require('crypto');
		var sha1 = crypto.createHash('sha1');
		sha1.update(content);
		return sha1.digest();
	};

	// 以4M为单位分割
	var sliceSize = 4*1024*1024;
	var bufferSize = buffer.length;
	var blockCount = Math.ceil(bufferSize / sliceSize);
	var prefix = 0x16;

	var sha1String = [];

	for(var i=0;i<blockCount;i++){
		sha1String.push(sha1(buffer.slice(i*sliceSize,(i+1)*sliceSize)));
	}

	var sha1Buffer = Buffer.concat(sha1String,blockCount * 20);

	// 如果大于4M，则对各个块的sha1结果再次sha1
	if(blockCount > 1){
		prefix = 0x96;
		sha1Buffer = sha1(sha1Buffer);
	}

	sha1Buffer = Buffer.concat(
		[new Buffer([prefix]),sha1Buffer],
		sha1Buffer.length + 1
	);

	return sha1Buffer.toString('base64')
		.replace(/\//g,'_').replace(/\+/g,'_');

}

module.exports = getEtag;
