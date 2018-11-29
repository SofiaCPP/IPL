#ifndef BFI_H
#define BFI_H

#include<iostream>
#include<stack>
#include<vector>
#include<iomanip>

class BFI
{
private:
    static const unsigned MEMORY_SIZE = 2048;
    std::string mCode;
    unsigned mIP, mMemoryUsed;
    unsigned char mMemory[MEMORY_SIZE];
    int mMP;
    std::stack<unsigned> mStack;
    std::vector<unsigned> mBrackets;
    
    void moveMPLeft();
    void moveMPRight();
    void incCellAtMP();
    void decCellAtMP();
    void printCellAtMP();
    void readToCellAtMP();
    void loopStart();
    void loopEnd();

public:
    BFI(const std::string& code) :
        mCode(code), mIP(0), mMP(0), mMemoryUsed(0),mMemory{0}
    {
        mBrackets.reserve(mCode.length());
    }
    void interpret();
    void printUsedMemory();
};

#endif // BFI_H
