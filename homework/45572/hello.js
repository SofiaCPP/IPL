// "Asdasdasd "
/* this is a testing*/
	/*comment
	on
	more 'than'
	one "line some text" *on it
	citation
	xD*********/
	*/

var fs = require('fs'),
    http = require('http'),
    URL = require('url'),
    crypto=require('crypto');

var currentPageName = './CardGame/pages/index.html';

//deleting the magic numbers
var battleIndex = 5,
    boonIndex = 6,
    castIndex = 7,
    modIndex = 9,
    updateIndex = 10,
    uniqCardsIndex = 11;

function main(){ 
 var server = http.createServer(function(req, res){
        var headers  = req.headers,
            method = req.method,
            url = req.url,
            body = [];

        var pathName = URL.parse(url).pathname;
   //     console.log('Request for ' + pathName + ' recieved');
        if(pathName == '/')
            console.log(pathName);

        req.on('error', function(err){
            console.error(err);
        }).on('data', function(chunk){
            body.push(chunk);
            
        }).on('end', function(){
            body = Buffer.concat(body).toString();
        
            res.on('error', function(err) {
                console.error(err);
            });
            
            res.statusCode = 200;
            res.setHeader('Content-Type', 'application/json');

            if(pathName == '/'){
                fs.readFile(currentPageName, function (err, html) {
                    if(err)
                        throw err;

                    res.writeHeader(200, {'Content-Type': 'text/html'});
                    res.write(html);
                    res.end();
                });
            }

            if(pathName.substring(pathName.length- 5) == '.html'){
                fs.readFile('./CardGame/pages' + pathName, function (err, html) {
                    if(err)
                        throw err;

                    res.writeHeader(200, {'Content-Type': 'text/html'});
                    res.write(html);
                    res.end();
                });
            }

            if(pathName.substring(pathName.length-3) == '.js'){
                fs.readFile('./CardGame/' + pathName, function (err, data){
                    if(err)
                        throw err;

                    res.write(data);
                    res.end();
                });
            }

            if(pathName.substring(pathName.length-4) == '.png' || pathName.substring(pathName.length-4) == '.jpg'){
                fs.readFile('./CardGame'+pathName, function(err,data){
                    if(err)
                        throw err;

                    res.write(data);
                    res.end();
                });
            }
        });
    }).listen(1234); 
    
    var io = require('socket.io')(server),
        sequence = 1,
        clients = [];
        
    var database = JSON.parse(fs.readFileSync('betaDummy.JSON'));
    var decks = JSON.parse(fs.readFileSync('decks.JSON'));
    var characters = JSON.parse(fs.readFileSync('characters.json'));
    var playGame = [];
    var inGame=[];
    
    io.on('connection', function (socket){
        console.info('New client connected (id= '+ socket.id + ').');
        clients.push(socket);

        socket.on('disconnect', function(){
            // remove from connected clients
            var index = clients.indexOf(socket);
            if(index != -1){
                clients.splice(index, 1);
                console.info('Client gone (id= ' + socket.id+').');

                // look if he is in game or in queue
                
                for(var i = 0; i < inGame.length; i+=1){
                    if(inGame[i][2].socket == socket){
                        inGame[i][3].socket.emit('enemySurrendered');
                        inGame.splice(i, 1);
                        console.log("yo left someone");
                        break;
                    }
                    else if(inGame[i][3].socket == socket){
                        inGame[i][2].socket.emit('enemySurrendered');
                        inGame.splice(i, 1);
                        console.log("Yo left someone");
                        break;
                    }
                }
            }

            // update database
            fs.writeFileSync('betaDummy.json', JSON.stringify(database));
            fs.writeFileSync('decks.json', JSON.stringify(decks));
            fs.writeFileSync('characters.json', JSON.stringify(characters));
        });

        var isLoggedIn;

        socket.emit('serverMessage', {message:'message is this'});

        socket.on('LogIn', function(data){
            console.log('LOGGGING.........');
            var emailIndex =findElement(database['email'],data.email),
                emailPassword = database['password'][emailIndex],
                username = database['username'][emailIndex];
            
            // the email does not exist return login has failed
            if(emailIndex == undefined){
;                console.log(data.email + ' - ' + emailIndex);
                socket.emit('LogConfirm', {isLoggedIn: isLoggedIn});
                return;
            }
            
            crypto.pbkdf2(data.password,emailPassword.salt,
                               emailPassword.iterations, 512, 'sha512',
                              function(err, key){
                if(key.toString('hex') == emailPassword.hash){
                    isLoggedIn = true;
                    var userId = '#' + database['uid'][emailIndex];
                
                    database[username+userId]['state']=
                        database[username+userId]['lastState'];
            
                    socket.emit('LogConfirm', {isLogged: isLoggedIn});
                    socket.emit('userData', {state:database[username+userId]['lastState'],
                                username: username, userID: userId,
                                profile: database[username+userId],
                                chars: characters[username+userId],
                                createdDecks: decks[username+userId]});
                }
                
                else
                    socket.emit('LogConfirm', {isLogged: isLoggedIn});
            });
            
            if(!isLoggedIn)  
                return;
        });
                
        socket.on('Register', function(data){
            var emailIndex =findElement(database['email'],data.email); 
            
            // check if email already exists if it does register has failed
            if(emailIndex || emailIndex == 0){//because 0 is false 
               socket.emit('registerFailed', {});
                return;
            }
            // 1) generate the salt
            // 2) decide how much iterations the new hash is
            // 3) save the salt, iterations and the created hash
            var salt = crypto.randomBytes(128).toString('base64');
            var iterations = 10000;
            crypto.pbkdf2(data.password, salt,iterations,512, 'sha512', 
                          function(err, key){
                if(err)
                    throw err;
                database['password'].push({salt:salt,iterations:iterations,
                                           hash:key.toString('hex')});
            }); 
            
            database['email'].push(data.email);
            database['username'].push(data.username);
            database['secretQuestion'].push(data.secretQuestion);          
            var userId;
                   
            while(!userId || database[data.username + '#' + userId]){
                    userId = 
                        Math.round(Math.random()* 9) +
                        Math.round(Math.random()* 9) * 10 + 
                        Math.round(Math.random()* 9) * 100 + 
                        Math.round(Math.random()* 9) * 1000;
            }
               
            database['uid'].push(userId);
            database[data.username + '#' + userId] = {friends:[], requestsFrom:[], 
                                level:0, wins:0, state:'offline',lastState:''};
            characters[data.username + '#'+ userId] = [];
            decks[data.username + '#'+ userId] = [];
            console.log(data.email + ' EM_PAS ' + data.password);
        });
        
        socket.on('changeHtmlPage', function(data){
            console.log('switch to ' + data.pageName);
            currentPageName = data.pageName;
        });

        socket.on('updateUserData', function(data){
            socket.emit('newUserData', {profile:database[data.username+data.userID]});
        });
        
        socket.on('CreatedDeck', function(data){
            console.log(data.deck);
            console.log(data.deckIndex);
            
                decks[data.username+data.userID].push({name: data.deckName, deck: data.deck, isUsable: data.isUsable});
        });
        
        socket.on('CH01', function(from, msg){
            console.log('MSG', from, ' saying ' , msg);
        });
        
        socket.on('CreatedCharacters', function(data){
            console.log(data.username + ' ' + data.userID);    
            
            characters[data.username+data.userID].push({name:data.name,level:data.level,exp:data.exp, spells:data.spells});
        });
        
        socket.on('changedCharacter', function(data){
            console.log(data.newCharacter);
            characters[data.username+data.userID][data.charIndex]=data.newCharacter;
        });

        socket.on('changedDeck', function(data){
            decks[data.username+data.userID][data.deckIndex]=data.deck;
            console.log(data.deck);
        });

        socket.on('SendMessage', function(data){
           var chatDb = database['chat'],
               chatHeading = data.sender + data.senderID + '-' + data.deliverTo;
            
           console.log('1)' + data.sender +data.senderID + '-' + 
                       data.deliverTo);
           console.log('2)' + data.deliverTo + '-' + data.sender+ 
                       data.senderID);
            
           if(!chatDb[chatHeading]){
               chatHeading =data.deliverTo+ '-' +data.sender +data.senderID; 
               chatDb[chatHeading].push(data.sender + ': ' + data.message);
           }
            
           else{
               chatDb[chatHeading].push(data.sender+': ' + data.message);
           }
            
            socket.emit('AllChats', {chat:chatDb[chatHeading]});
        });
        
        socket.on('RequestFriendProfile', function(data){
            console.log(database[data.friend]);
            socket.emit('FriendProfile', {friendProfile:database[data.friend]});
        });
        
        socket.on('SendFriendRequest', function(data){
        var hasAlreadySent = findElement(database[data.reciever]['requestsFrom'],   
                                         data.username+data.userID);
            
          if(!hasAlreadySent && hasAlreadySent != 0){
              database[data.reciever]['requestsFrom'].push(data.username+data.userID);
          }
        });
        
        socket.on('AcceptFriendRequest', function(data){
            var nameAt = findElement(database[data.concordant]
                                     ['requestsFrom'], data.requester);
             database[data.concordant]['requestsFrom'].splice(nameAt, 1);
             database[data.concordant]['friends'].push(data.requester);
             database[data.requester]['friends'].push(data.concordant);
             database['chat'][data.requester + '-' + data.concordant] = [];    
        });
        
        socket.on('ChangeState', function(data){
            database[data.username+data.userID]['state']=data.state;
        });
        
        socket.on('StateLoggedOut', function(data){
            
            database[data.username+data.userID]['state']= 'offline';
            database[data.username+data.userID]['lastState'] = data.state;
        });
        
        //play Game
        socket.on('QueuePlayGame', function(data){
            //add the player to the queue and after an interval search for an opponent
            playGame.push(data.username+data.userID);
            console.log('QUEUED ' + data.username+data.userID);
            // after the interval the queue takes actions
            setInterval(function(){
                var isOpponentFound = false,
                    length = playGame.length,
                    user = data.username+data.userID,
                    queueIndex = findElement(playGame, data.username+data.userID);

                    // if he is not found in the queue, he is inGame
                    if(queueIndex == undefined){
                        console.log('he is inGame');
                        for(var i = 0; i<inGame.length; i+=1){
                            if(inGame[i][0] == user || inGame[i][1] == user){
                                isOpponentFound = true;

                                if(inGame[i][0] == user)
                                    inGame[i][2].socket = socket;
                                else
                                    inGame[i][3].socket = socket;
                                    
                                
                                console.log(inGame[i][2].socket.id + ' vs ' + inGame[i][3].socket.id);
                                // there is a gurantee that this is the late arival player
                            }
                        }
                    }
                
                    // if the user if the only one queued he is kicked out
                    else if(length == 1){
                        console.log('was only');
                        isOpponentFound = false;
                        playGame.pop();
                    }

                    // if there is another user, start a game with him
                    else if(length > 1){
                        isOpponentFound = true;
                        var p1Data = {playerFields:['','','','','',''],playerHand:[], socket,
                                         characterClass:'', deck:[], fieldIndex:NaN, handIndex:NaN, cardName:''},
                            p2Data = {playerFields:['','','','','',''], playerHand:[], socket,
                                         characterClass:'', deck:[], fieldIndex:NaN, handIndex:NaN, cardName:''};
                                
                        // to determine xthe first Player
                        var coinFlip = Math.round(Math.random() * 100);
                        if(coinFlip > 50){
                            console.log('inGame n');
                            var randomMode = Math.round(Math.random() * 1);
                            var playMode = 'Normal';                            
                            // 0 is for normal

                            if(randomMode == 1)
                                playMode = 'Dominion';

                            inGame.push([playGame[queueIndex+1],user,p1Data,p2Data,true, {}, 
                            {cardCan:'',onField:NaN, onIndex:NaN,changingFieldIndex:NaN},{},'Main Battle', playMode,
                        {updateCounter:0,updateUniqCounter:0}, []]);

                             // add to p2Data user's socketw
                             p2Data.socket = socket;
                        }
                        
                        // true stands for playerOne is on turn
                        else{ //0,1:names; 2,3:datas; 4:isP1turn; 5:battles; 6:cardSpecial; 7:spells, 8:Battle's Location
                            console.log('inGame y');
                            var randomMode = Math.round(Math.random() * 0);
                            var playMode = 'Normal';
                            // 0 is for normal
                            if(randomMode == 1)
                                playMode = 'Dominion';

                            inGame.push([playGame[queueIndex+1],user,p1Data,p2Data,true, {}, 
                                {cardCan:'',onField:NaN, onIndex:NaN,changingFieldIndex:NaN},{},'Main Battle', playMode,
                            {updateCounter:0, updateUniqCounter:0}, []]);

                             // add to p1Data user's socket
                             p1Data.socket = socket;
                        }
                        playGame.splice(queueIndex, 2);
                    }
                    
                socket.emit('matchFound',{isFound: isOpponentFound});
                clearInterval(this);
            }, 10000);
            
            socket.on('initGame', function(data){
                    index={gameOrder:undefined,player:undefined},
                    user = data.username+data.userID;
                
                for(var i = 0; i < inGame.length; i+=1){
                    if(inGame[i][0] == user){
                        index.gameOrder = i;
                        index.player = 0;
                        break;
                    }
                    
                    else if(inGame[i][1] == user){
                        index.gameOrder = i;
                        index.player = 1;
                        break;
                    }
                }
                
                var currentGame = inGame[index.gameOrder];
                
                currentGame[index.player+2]['characterClass'] = data.characterClass;
                currentGame[index.player+2]['deck'] = data.deck;
                socket.emit('placement', {gameOrder:index.gameOrder,player:index.player, playMode:currentGame[modIndex]});
            });
        });
        
        var isChanged;
        // read the gameDatas, find playerData and add his card
        socket.on('spawnCard', function(data){
            isChanged = true;
            var currentGame = inGame[data.gameOrder];
            currentGame[data.playerIndex+2].playerFields[data.fieldIndex] = data.cardName;
            currentGame[data.playerIndex+2].fieldIndex = data.fieldIndex;
            currentGame[data.playerIndex+2].cardName = data.cardName
            currentGame[data.playerIndex+2].handIndex=data.handIndex;
            
            var enemyData;
            if(data.playerIndex == 0)
                enemyData = 3;
            else
                enemyData = 2;
            
            currentGame[updateIndex].updateCounter++;                            
        //    console.log('Enemy socket: ' + currentGame[enemyData].socket.id  + '\n' +
          //  'Player socket: ' + currentGame[data.playerIndex+2].socket.id); 
        });
        
        socket.on('battle', function(data){
            isChanged = true;
            var currentGame = inGame[data.gameOrder];
            var battleInfo = {defenderIndex:data.defenderIndex,attackerIndex:data.attackerIndex,
                defenderField:data.defenderField};
            currentGame[battleIndex]=battleInfo;


            var enemyData;
            if(data.playerIndex == 0)
                enemyData = 3;
            else
                enemyData = 2;

            currentGame[updateIndex].updateCounter++;                
       //      currentGame[enemyData].socket.emit('enemyBattle',{battleInfo});
          });

        socket.on('setPrisoner', function(data){
            var currentGame = inGame[data.gameOrder];
            var enemyData;

            if(data.playerIndex == 0)
                enemyData = 3;
            else
                enemyData = 2;

            console.log('GAMEORDer' + data.gameOrder);
            console.log('PRISONER: ' + data.prisonerIndex);
            console.log(currentGame.length);
            console.log('Enemy socket: ' + currentGame[enemyData].socket.id  + '\n' +
             'Player socket: ' + currentGame[data.playerIndex+2].socket.id);

            currentGame[enemyData].socket.emit('enemySetPrisoner', {prisonerIndex: data.prisonerIndex}, function(fn){
                console.log('sent');
            });
        });

        socket.on('spellCasted', function(data){
            var currentGame = inGame[data.gameOrder];
            var spellInfo = {markedCursor:data.markedCursor, character:data.character,
                cursorPosition:data.cursorPosition, spellIndex: data.spellIndex};
            currentGame[castIndex]=spellInfo;
 

            var enemyData;
            if(data.playerIndex == 0)
                enemyData = 3;
            else
                enemyData = 2;

            currentGame[updateIndex].updateCounter++;            
      //      currentGame[enemyData].socket.emit('enemySpellsCasted', {enemySpellsCasted:spellInfo});     
        });

        socket.on('cardCan', function(data){
            isChanged = true;
            var currentGame = inGame[data.gameOrder];
            currentGame[boonIndex].cardCan = data.cardCan;
            currentGame[boonIndex].onField = data.onField;
            currentGame[boonIndex].onIndex = data.onIndex;
            currentGame[boonIndex].changingFieldIndex = data.changingFieldIndex;

            var enemyData;
            if(data.playerIndex == 0)
                enemyData = 3;
            else
                enemyData = 2;
                
            currentGame[updateIndex].updateCounter++;            
       /*     currentGame[enemyData].socket.emit('enemyCardEffect', {
                                onField:currentGame[boonIndex].onField,
                                 onIndex:currentGame[boonIndex].onIndex,  cardCan:currentGame[boonIndex].cardCan,
                                changingFieldIndex:currentGame[boonIndex].changingFieldIndex});     
       */});

        socket.on('uniqActions', function(data){
            var currentGame = inGame[data.gameOrder];
            var playerIndex = data.playerIndex;

            currentGame[uniqCardsIndex] = data.uniqActions;
            currentGame[updateIndex].updateUniqCounter++;

        });

        socket.on('playerRequestData', function(data){
            var currentGame = inGame[data.gameOrder];
            var playerIndex = data.playerIndex;
            var enemyIndex = 0;
            if(playerIndex == 0)
                enemyIndex = 1;

            if(!currentGame[enemyIndex])
                return;

            var playerData = currentGame[enemyIndex+2];
            
            console.log(inGame.length);

            if(currentGame[playerIndex+2].socket){
               // console.log("UpdateIndex " + currentGame[updateIndex].updateCounter);

                socket.emit('battleReport'+currentGame[data.playerIndex], {enemyFields:playerData.playerFields,
                    shandIndex:playerData.handIndex,
                    fieldIndex:playerData.fieldIndex, cardName:playerData.cardName, /*<-spawnCard*/
                defenderIndex:currentGame[battleIndex].defenderIndex, attackerIndex:currentGame[battleIndex].attackerIndex,
                defenderField:currentGame[battleIndex].defenderField, /*<-battle*/ markedCursor:currentGame[castIndex].markedCursor,
                character:currentGame[castIndex].character, cursorPosition:currentGame[castIndex].cursorPosition, 
                spellIndex:currentGame[castIndex].spellIndex, /*<-spellCast*/ cardCan:currentGame[boonIndex].cardCan,
                onField:currentGame[boonIndex].onField, onIndex: currentGame[boonIndex].onIndex, 
                changingFieldIndex:currentGame[boonIndex].changingFieldIndex, /*updateCounter->*/
                 updateCounter:currentGame[updateIndex].updateCounter
                }, function(data){
                    data.enemyFields = ['','','','','',''];
                    data.playerFields = ['','','','','',''];
                    playerData.fieldIndex = NaN;
                    currentGame[battleIndex] = {};
                    currentGame[boonIndex] = {};
                    currentGame[castIndex]={};
                });

                socket.on('updateBattlefield', function(){
                    // data.enemyFields = ['','','','','',''];
                    // data.playerFields = ['','','','','',''];
                    // playerData.fieldIndex = NaN;
                })
            }
        });

        socket.on('checkUniqActions', function(data){
            var currentGame = inGame[data.gameOrder];
            var playerIndex = data.playerIndex;
        
            if(!currentGame)
                return;

                socket.emit('recieveUniqActions', {uniqActions:currentGame[uniqCardsIndex], 
                    updateCounter:currentGame[updateIndex].updateUniqCounter});
        });

        // add ended Player data and get game[4](isPlayer's one turn)
        socket.on('endTurn', function(data){ 
            var currentGame = inGame[data.gameOrder];
            
            if(data.playerIndex == 0)
                currentGame[4]=false;
            else
                currentGame[4]=true;
         
            currentGame[data.playerIndex+2].playerTauntsOnFiled=data.playerTauntsOfFiled;
            currentGame[data.playerIndex+2].playerHand=data.playerHand;      
            currentGame[battleIndex] = [];
            currentGame[castIndex] = [];
        });
        // recieve ended player data
        socket.on('requestStartTurn', function(data){
           // console.log(data.gameOrder);

            var currentGame = inGame[data.gameOrder]; 
            
            if(!currentGame)
                return;

            var enemyData = currentGame[2];
            var enemyIndex = 0;
            
            if(data.playerIndex == 0){
                enemyData = currentGame[3];
                enemyIndex=1;
            }
            
            if((currentGame[4] && data.playerIndex == 0) ||
               (!currentGame[4] && data.playerIndex == 1)){

                socket.emit('startTurn'+currentGame[data.playerIndex], 
                    {attackedFromFields:currentGame[enemyIndex].attackedFromFields,
                        playerHand:currentGame[enemyIndex].playerHand, 
                        updateCounter:currentGame[updateIndex].updateCounter});
            }
        });

        socket.on('surrender', function(data){
            var enemyIndex;

            if(data.playerIndex == 0)
                enemyIndex = 1;
            else
                enemyIndex = 0;
    
            if(inGame[data.gameOrder] == undefined)
                return;
                
            inGame[data.gameOrder][enemyIndex+2].socket.emit('enemySurrendered');
         });
  
         socket.on('recievedSurrender', function(data){
            console.log('Calling  to shut down the game server');
            var currentGame = inGame[data.gameOrder]; 
            var enemyIndex;

            if(data.playerIndex == 0)
                enemyIndex = 1;
            else
                enemyIndex = 0;

            inGame.splice(data.gameOrder, 1); 
            console.log(inGame.length + ' inGameLength');
        }); 
    });
    console.log('Server is running on port 1234');
}

function endGame(data){
}

function findElement(array, element){
    var i,
        length = array.length;
    
    for(i = 0; i < length; i+=1){
        if(array[i] == element)
            return i;
    }
}