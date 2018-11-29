#include "BFI.h"

void BFI::moveMPLeft()
{
    --mMP;
}
    
void BFI::moveMPRight()
{
    ++mMP;
    if(mMP > mMemoryUsed) mMemoryUsed = mMP;
}
    
void BFI::incCellAtMP()
{
    if(mMP >= 0 && mMP < MEMORY_SIZE)
    {
        ++mMemory[mMP];
    }
    else
    {
        std::cout << "Out of memory bounds!!! MP: " << mMP << " IP: " << mIP << " " << mCode[mIP] << std::endl;
        exit(1);
    }
}
    
void BFI::decCellAtMP()
{
    if(mMP >= 0 && mMP < MEMORY_SIZE)
    {
        --mMemory[mMP];
    }
    else
    {
        std::cout << "Out of memory bounds!!! MP: " << mMP << " IP: " << mIP << " " << mCode[mIP] << std::endl;
        exit(1);
    }
}
    
void BFI::printCellAtMP()
{
    if(mMP >= 0 && mMP < MEMORY_SIZE)
    {
        if(mMemory[mMP] == 10)
        {
            std::cout<<"\n";
        }
        else
        {
            std::cout<<mMemory[mMP];
        }
    }
    else
    {
        std::cout << "Out of memory bounds!!! MP: " << mMP << " IP: " << mIP << " " << mCode[mIP] << std::endl;
        exit(1);
    }
}
    
void BFI::readToCellAtMP()
{
    unsigned char c;
    std::cin>>c;
    if(mMP >= 0 && mMP < MEMORY_SIZE)
    {
        mMemory[mMP] = c;
    }
    else
    {
        std::cout << "Out of memory bounds!!! MP: " << mMP << " IP: " << mIP << " " << mCode[mIP] << std::endl;
        exit(1);
    }
}
void BFI::loopStart()
{
    if(mMP >= 0 && mMP < MEMORY_SIZE)
    {
        if(mMemory[mMP] == 0)
        {
            mIP = mBrackets[mIP];
        }
    }
    else
    {
        std::cout << "Out of memory bounds!!! MP: " << mMP << " IP: " << mIP << " " << mCode[mIP] << std::endl;
        exit(1);
    }
}
    
void BFI::loopEnd()
{
    if(mMP >= 0 && mMP < MEMORY_SIZE)
    {
        if(mMemory[mMP] != 0)
        {
            mIP = mBrackets[mIP];
        }
    }
    else
    {
        std::cout << "Out of memory bounds!!! MP: " << mMP << " IP: " << mIP << " " << mCode[mIP] << std::endl;
        exit(1);
    }
}

void BFI::interpret()
{
    while(mCode[mIP] != '\0')
    {
        if(mCode[mIP] == '[')
        {
            mStack.push(mIP);
        }
        else if(mCode[mIP] == ']')
        {
            if(mStack.empty())
            {
                std::cout << "Unexpected closing bracket!!!\n";
                exit(1);
            }
            mBrackets[mIP] = mStack.top();
            mBrackets[mStack.top()] = mIP;
            mStack.pop();
        }
        ++mIP;
    }
    if(!mStack.empty())
    {
        std::cout << "Unclosed opening bracket!!!\n";
        exit(1);
    }
    mIP = 0;
    while(mCode[mIP] != '\0')
    {
        switch(mCode[mIP])
        {
            case '>': moveMPRight();    break;
            case '<': moveMPLeft();     break;
            case '+': incCellAtMP();    break;
            case '-': decCellAtMP();    break;
            case '.': printCellAtMP();  break;
            case ',': readToCellAtMP(); break;
            case '[': loopStart();      break;
            case ']': loopEnd();        break;
        }
        ++mIP;
    }
}

void BFI::printUsedMemory()
{
    unsigned cols = 16;
    unsigned rows = mMemoryUsed/cols;
    unsigned lastRow = mMemoryUsed%cols;
    unsigned ind = 0;
    std::cout.fill('0');
    for(unsigned i = 0; i < rows; ++i)
    {
        std::cout << std::setw(4) << ind << ": ";
        for(unsigned j = 0; j < cols; j++)
        {
            std::cout << std::setw(3) << (unsigned)mMemory[ind++] << " ";
        }
        std::cout << std::endl;
    }
    if(lastRow != 0)
    {
        std::cout << std::setw(4) << ind << ": ";
        while(ind <= mMemoryUsed)
        {
            std::cout << std::setw(3) << (unsigned)mMemory[ind++] << " ";
        }
        std::cout << std::endl;
    }
}
